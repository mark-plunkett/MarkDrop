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
    PreviousSamples: float list list
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
    let fftOutputSize = float fftBlockSize * fftOutputRatio
    let fftBlockSizeBytes = fftBlockSize * wavHeader.BlockAlign
    let scalingCoefficient = 8. * pown 2. 16
    let invScalingCoefficient = 1. / scalingCoefficient
    let yScalingFactor = 0.5 * pown 2. 16
    let slope = 5.
    let slopeScale = slope / float canvas.Width
    let smoothing = 2

    let logScale value =
        let divisor = log10 <| (float fftOutputSize)
        let o = (fftOutputSize * log10 (max 1. value)) / divisor
        o

    let scaleX value = 
        value
        |> logScale
        |> WaveformViz.scale (float canvas.Width / fftOutputSize) 
        |> float

    let translateY value = int canvas.Height - 1 - value

    let applySlope xPos value =
        value * xPos * slopeScale

    let scaleY xPos value = 
        value * float fftBlockSize * invScalingCoefficient * float canvas.Height
        |> applySlope xPos

    let fftUserStateAggregator oldData newData =
        { oldData with SampleBytes = Array.append oldData.SampleBytes newData }

    let chunkBytes bytes =
        match Array.length bytes with
        | length when length < fftBlockSizeBytes -> 
            let zeroed = Array.zeroCreate fftBlockSizeBytes
            Array.blit bytes 0 zeroed 0 length
            (zeroed, [||])
        | _ -> Array.splitAt fftBlockSizeBytes bytes

    let aggregateSamples (samples: int[,]) =
        // TODO: this is only using left channel atm
        samples.[0,*]

    let normalizeSamples (samples: int[]) =
        samples |> Array.map (fun i -> float i / yScalingFactor)

    let throttleFps frameState = 
        let targetFps = 60.
        let elapsedSeconds = (float frameState.ElapsedMs / 1000.)
        let diff = float frameState.FrameCount - (targetFps * elapsedSeconds) |> int
        if diff > 0 then
            Threading.Thread.Sleep(diff)

    let fftAnimator canvas frameState = 

        let drawSpectrum samples = 
            samples
            |> Array.map (fun s -> System.Numerics.Complex(s, 0.))
            |> FFFT.fft 
            |> Array.take (float (Array.length samples) * fftOutputRatio |> int)
            |> Array.skip 1
            |> Array.map (fun c -> c.Real)
            |> Array.mapi (fun i v -> 
                let xPos = i |> float |> scaleX
                let yTop = v |> abs |> scaleY xPos |> int |> translateY
                Drawille.pixel (int xPos) yTop)
            |> Util.flip Drawille.drawTurtle (canvas |> Drawille.clear)
            |> ConViz.updateConsole convas

        let storeSamples previousSamples samples =
            match List.length previousSamples with
            | length when length = smoothing -> samples :: previousSamples.Tail
            | _ -> samples :: previousSamples

        let smooth previousSamples samples = 
            samples :: previousSamples
            |> List.transpose
            |> List.map List.average

        let rec processBlock animationState =

            throttleFps frameState |> ignore

            match Array.length animationState.SampleBytes with
            | length when length < fftBlockSizeBytes -> 
                animationState
            | _ ->
                let (bytes, nextBytes) = chunkBytes animationState.SampleBytes
                let samples = 
                    WavAudio.bytesToSamples sampleInfo bytes
                    |> aggregateSamples
                    |> normalizeSamples

                samples
                |> Array.toList
                |> smooth animationState.PreviousSamples
                |> Array.ofList
                |> drawSpectrum 
                |> ignore

                let userState' = { 
                    animationState with 
                        TotalBytesProcessed = animationState.TotalBytesProcessed + animationState.SampleBytes.Length
                        SampleBytes = nextBytes
                        PreviousSamples = storeSamples animationState.PreviousSamples <| List.ofArray samples
                }
                match nextBytes with
                | [||] -> userState'
                | _ -> processBlock userState'

        processBlock frameState.UserState

    let initialUserState = {
        SampleBytes = Array.zeroCreate 0
        TotalBytesProcessed = 0
        PreviousSamples = []
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
            //let msToWait = viz.PostAndReply (fun replyChannel ->  Reply(replyChannel)) |> calculateLatency |> max 0.
            do! Async.Sleep (int 5.) //(int msToWait)

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
    //let fileName = @"D:\Google Drive\Music\flac\FC Kahuna\Machine Says Yes\(1) Hayling.wav"
    let fileName = @"C:\Dev\MarkDrop\Audio\JANICE - b - 1.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\silence.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\kicks-sparse.wav"

    if argv.[0] = "-w" then

        drawwaveform fileName

    else if argv.[0] = "-v" then

        //SimpleViz.drawRawFFT fileName
        asyncFFT fileName

    0