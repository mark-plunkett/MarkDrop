open System
open NAudio
open ConViz
(*
Args
    
    -f|--file       Path to file, currently only .wav is supported
    -w|--waveform   Displays the waveform of the input file
    --display-mono  Displays the waveform as a mono sum


*)

type Args = {
    FileName: string

}

let parseArgs args =

    {FileName = ""}

let drawwaveform fileName =

    let wavHeader = WavAudio.readHeader fileName false
    wavHeader |> WavAudio.printInfo fileName

    let convas = ConViz.initialise
    let canvas = Drawille.createPixelCanvas (convas.CharWidth * 2) (convas.CharHeight * 4)
    let samplesPerChunk = WaveformViz.getChunkSize wavHeader canvas    

    WavAudio.parallelProcessAllData fileName samplesPerChunk
    |> WaveformViz.drawMonoSumWaveform canvas (float Int16.MaxValue)
    |> Drawille.toStrings
    |> printfn "%s"
    
    ()

type AnimationState = {
    SampleBytes: int[]
}

let animateRect viz =

    let convas = ConViz.initialise
    let canvas = Drawille.createCharCanvas convas.CharWidth convas.CharHeight

    let rectAnimator canvas state =
        ConViz.rotateRect canvas state
        |> ConViz.updateConsole convas
        state.UserState

    let rectUserStateAggregator oldData newData =
        match newData with
        | Some data -> data
        | None -> oldData

    let viz = Vizualizer<float>((rectAnimator canvas), rectUserStateAggregator, 1.).Start

    let rec feed f =

        viz.Post f 
        Threading.Thread.Sleep 500
        feed <| f + 1.

    feed 1. |> ignore

let asyncFFT fileName =

    (*
        Setup async animation loop that processes data according to the sample rate of the source.
        Data will be provided in chunks of some size which is a multiple of BlockAlign (usually 4)
    
        Data should be concat'ed and provided to a processor, which can
            - optionally choose to process the data when enough data is available
            - return a new state, minus the processed data
    *)

    let fftBlockSize = pown 2 12
    let convas = ConViz.initialise
    let canvas = Drawille.createCharCanvas convas.CharWidth (convas.CharHeight / 2)

    let wavHeader = WavAudio.readHeader fileName false
    let wavData = WavAudio.readData fileName wavHeader
    let sampleInfo = WavAudio.getSampleInfo wavHeader

    let fftUserStateAggregator oldData newData =
        match newData with
        | Some data -> Array.append oldData data 
        | None -> oldData

    let fftAnimator (canvas: Drawille.Canvas) frameState = 

        let rec processBlock sampleBytes =

            match sampleBytes with
            | [||] -> sampleBytes
            | _ ->

                let (data, next) = 
                    match Array.length sampleBytes with
                    | x when x < fftBlockSize -> (sampleBytes, [||])
                    | _ -> Array.splitAt fftBlockSize sampleBytes

                let bytes = 
                    match Array.length data with
                    | x when x < fftBlockSize ->
                        let zeroed = Array.zeroCreate fftBlockSize
                        Array.blit data 0 zeroed 0 (Array.length data)
                        zeroed
                    | _ -> data

                let samples = WavAudio.bytesToSamples sampleInfo bytes
                //printfn "%i" <| Array2D.length2 samples

                // TODO: this is only using left channel atm
                let output = 
                    FFT.fftList (samples.[0,*] |> Array.map float |> List.ofArray) 
                    |> List.mapi (fun i x -> 
                        match i with
                        | 0 -> 0.
                        | _ -> abs x
                    )

                output
                |> WaveformViz.drawWaveformScaled (canvas |> Drawille.clear) 4000000.
                |> ConViz.updateConsole convas

                processBlock next

        processBlock frameState.UserState

    let viz = Vizualizer((fftAnimator canvas), fftUserStateAggregator, Array.zeroCreate 0).Start
    
    (*
        Read file in chunks of s samples, prepare data in b buffers and feed into viz at a rate of latency ms 

    *)

    let latencyMs = 50
    let numBuffers = 1
    let numSamples = wavHeader.SampleRate / latencyMs
    let numBytesRaw = (wavHeader.BitsPerSample * numSamples) / 8
    let numBytes = numBytesRaw - (numBytesRaw % wavHeader.BlockAlign)
    let dataLength = Array.length wavData

    let rec queueSamples sampleOffset threadNumber = async {

        let byteOffset = (sampleOffset * wavHeader.BitsPerSample) / 8
        match byteOffset with
        | x when x >= dataLength -> 
            ()
        | _ ->         
            let readLength = 
                match (byteOffset + numBytes) with
                | o when o > dataLength -> o - dataLength
                | _ -> numBytes

            let sampleBytes = Array.sub wavData byteOffset readLength
            viz.Post sampleBytes
            do! Async.Sleep 10
            return! queueSamples (sampleOffset + (threadNumber * numSamples)) threadNumber
    }

    [1..numBuffers]
    |> List.mapi (fun i n -> queueSamples (i * numSamples) n)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    ()

[<EntryPoint>]
let main argv =

    // resources
    // WAV format: https://web.archive.org/web/20141213140451/https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
    // FFT: https://en.wikipedia.org/wiki/Fast_Fourier_transform
    // Cooley–Tukey FFT: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

    //let fileName = @"C:\Dev\MarkDrop\Audio\single-sine.wav"
    // !!! 24BIT IS BROKEN
    //let fileName = @"D:\Google Drive\Production\Samples\# Synth Drums\unprocessed drums\toms\unusual toms\wasd_tom_sys100_ceramic-2_s_u.wav"
    // LONG FILE ~1GB
    // let fileName = @"D:\Google Drive\Music\Mixes\Jungle\Gold Dubs Revamped Classics Mix 2014.wav"


    //let fileName = @"C:\Dev\MarkDrop\Audio\sine-sweep.wav"
    //let fileName = @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav"
    let fileName = @"D:\Google Drive\Music\flac\FC Kahuna\Machine Says Yes\(1) Hayling.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\kicks-sparse.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\silence.wav"

    if argv.[0] = "-w" then

        drawwaveform fileName

    else if argv.[0] = "-v" then

        //SimpleViz.drawRawFFT fileName
        asyncFFT fileName

    0