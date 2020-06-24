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
    SampleBytes: byte[]
    TotalBytesProcessed: int
    PreviousSamples: float[][]
}

// let animateRect viz =

//     let convas = ConViz.initialise
//     let canvas = Drawille.createCharCanvas convas.CharWidth convas.CharHeight

//     let rectAnimator canvas state =
//         ConViz.rotateRect canvas state
//         |> ConViz.updateConsole convas
//         state.UserState

//     let rectUserStateAggregator oldData newData =
//         match newData with
//         | Some data -> data
//         | None -> oldData

//     let viz = Vizualizer((rectAnimator canvas), rectUserStateAggregator, 1.).Start

//     let rec feed f =

//         viz.Post f 
//         Threading.Thread.Sleep 500
//         feed <| f + 1.

//     feed 1. |> ignore

let asyncFFT fileName =

    (*
        Setup async animation loop that processes data according to the sample rate of the source.
        Data will be provided in chunks of some size which is a multiple of BlockAlign (usually 4)
    
        Data should be concat'ed and provided to a processor, which can
            - optionally choose to process the data when enough data is available
            - return a new state, minus the processed data
    *)

    let convas = ConViz.initialise
    let canvas = Drawille.createCharCanvas convas.CharWidth (convas.CharHeight / 2)

    let wavHeader = WavAudio.readHeader fileName false
    let wavData = WavAudio.readData fileName wavHeader
    let sampleInfo = WavAudio.getSampleInfo wavHeader

    let p = 11
    let fftBlockSize = pown 2 p
    let fftOutputRatio = 0.5 // We discard half the FFT output
    let fftPaddedSize = pown 2 13
    let fftOutputSize = fftOutputRatio * float fftPaddedSize //float fftBlockSize * fftOutputRatio
    let fftBlockSizeBytes = fftBlockSize * wavHeader.BlockAlign
    let yScalingFactor = 2.
    let sampleScalingFactor = 0.5 * pown 2. 16
    let slope = 4.
    let slopeScale = slope / float canvas.Width
    let smoothing = 3
    let skip = 2

    let scaleX value = 
        value
        |> Util.logScale fftOutputSize
        |> WaveformViz.scale (float canvas.Width / fftOutputSize) 
        |> float

    let translateY value = 
        int canvas.Height - 1 - value

    let applySlope xPos value =
        value * xPos * slopeScale

    let scaleY xPos value = 
        value * yScalingFactor
        |> applySlope xPos

    let fftUserStateAggregator oldData newData =
        { oldData with SampleBytes = Array.append oldData.SampleBytes newData }

    let aggregateSamples (samples: int[,]) =
        // TODO: this is only using left channel atm
        samples.[0,*]

    let throttleFps frameState = 
        let targetFps = 600.
        let elapsedSeconds = (float frameState.ElapsedMs / 1000.)
        let diff = float frameState.FrameCount - (targetFps * elapsedSeconds) |> int
        if diff > 0 then
            Threading.Thread.Sleep(diff)

    let fftAnimator canvas frameState = 

        let drawSpectrum samples = 
            samples
            |> FFFT.fftF2F
            |> Array.take (int fftOutputSize)
            |> Array.skip 20
            |> Array.mapi (fun i v -> 
                let i' = i - skip
                let xPos = i' |> float |> scaleX |> int
                let yPos = v |> abs |> scaleY <| float xPos |> int |> translateY
                Drawille.pixel xPos yPos)
            |> Util.flip Drawille.drawTurtle (canvas |> Drawille.clear)
            |> ConViz.updateConsole convas

        let storeSamples previousSamples samples =
            if smoothing = 0 then
                [||]
            else
                match Array.length previousSamples with
                | length when length = smoothing -> Array.append (Array.tail previousSamples) [|samples|]
                | _ -> Array.append previousSamples [|samples|]

        let smooth previousSamples samples = 

            previousSamples
            |> Array.append [|samples|]
            |> Array.transpose
            |> Array.map Array.average

        let rec processBlock animationState =

            throttleFps frameState |> ignore

            // match Array.length animationState.SampleBytes with
            // | length when length < fftBlockSizeBytes -> 
            //     animationState
            // | _ ->
            let (bytes, nextBytes) = Util.chunkBytes fftBlockSizeBytes animationState.SampleBytes
            let samples = 
                WavAudio.bytesToSamples sampleInfo bytes
                |> aggregateSamples
                |> Util.pad fftPaddedSize
                |> Util.normalize sampleScalingFactor

            samples
            |> smooth animationState.PreviousSamples
            |> drawSpectrum 
            |> ignore

            let userState' = { 
                animationState with 
                    TotalBytesProcessed = animationState.TotalBytesProcessed + animationState.SampleBytes.Length
                    SampleBytes = nextBytes
                    PreviousSamples = storeSamples animationState.PreviousSamples samples
            }
            match nextBytes with
            | [||] -> userState'
            | _ -> processBlock userState'

        processBlock frameState.UserState

    let initialUserState = {
        SampleBytes = Array.zeroCreate 0
        TotalBytesProcessed = 0
        PreviousSamples = Array.empty
    }
    let viz = Vizualizer((fftAnimator canvas), fftUserStateAggregator, initialUserState).Start
    
    // Set up data stream, animation will handle throttling

    let latencyMs = 30.
    let numBuffers = 1
    let numSamples = wavHeader.SampleRate / int latencyMs
    let numBytesRaw = (wavHeader.BitsPerSample * numSamples) / 8
    let numBytes = numBytesRaw - (numBytesRaw % wavHeader.BlockAlign)
    let dataLength = Array.length wavData
    let expectedBytesPerMs = (float sampleInfo.SampleRate * float sampleInfo.BytesPerMultiChannelSample) / 1000.

    let calculateLatency frameState =
        let expectedBytesProcessed = float frameState.ElapsedMs * expectedBytesPerMs
        (float frameState.UserState.TotalBytesProcessed - expectedBytesProcessed) / expectedBytesPerMs
        
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
            viz.Post (Data sampleBytes)
            let msToWait = viz.PostAndReply (fun replyChannel ->  Reply(replyChannel)) |> calculateLatency |> max 0. |> int
            do! Async.Sleep msToWait

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
    let fileName = @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav"
    //let fileName = @"D:\Google Drive\Music\flac\FC Kahuna\Machine Says Yes\(1) Hayling.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\JANICE - b - 1.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\silence.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\kicks-sparse.wav"

    if argv.[0] = "-w" then

        drawwaveform fileName

    else if argv.[0] = "-v" then

        //SimpleViz.drawRawFFT fileName
        asyncFFT fileName

    0