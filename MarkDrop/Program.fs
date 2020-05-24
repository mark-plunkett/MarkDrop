open System
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

    Console.OutputEncoding <- Text.Encoding.UTF8
    Console.CursorVisible <- false
    let originalCursorTop = Console.CursorTop
    let w = Console.WindowWidth
    let h = Console.WindowHeight
    let cursorEndY = originalCursorTop + (h / 2)
    
    let canvasWidth = (w * 2) - 2
    let canvasHeight = (h * 2) - 8
    let yPixelOffset = originalCursorTop * 4
    let canvas = Drawille.createOffsetPixelCanvas canvasWidth canvasHeight 0 yPixelOffset
    let convas = WaveformViz.convasFromCanvas canvas (pown (float 2) wavHeader.BitsPerSample)
    let samplesPerChunk = WaveformViz.getChunkSize wavHeader canvas
    
    //let printMinMaxParallelMapped fileName =
    //    WavAudio.parallelMapAllData fileName samplesPerChunk WaveformViz.minMaxValues2D
    //    |> Seq.map (fun (x, (min, max)) -> WaveformViz.minMaxToPixels convas x min max)
    //    |> Seq.fold WaveformViz.pointFolder canvas

    //let printMinMaxParallel fileName =
    //    WavAudio.parallelProcessAllData fileName samplesPerChunk
    //    |> WaveformViz.drawWaveformSamples convas canvas

    //printMinMaxParallel fileName
    //|> ignore

    WavAudio.parallelProcessAllData fileName samplesPerChunk
    |> WaveformViz.drawMonoSum convas canvas
    |> Drawille.toStrings
    |> printfn "%s"
    
    Console.CursorTop <- cursorEndY

type AnimationState = {
    SampleBytes: int[]
}

let animateRect viz =

    let convas = ConViz.initialise
    let canvas = Drawille.createPixelCanvas convas.CanvasWidth convas.CanvasHeight

    let rectAnimator canvas state =
        ConViz.rotateRect canvas state
        |> ConViz.updateConsole
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

    let convas = ConViz.initialise
    let canvas = Drawille.createPixelCanvas convas.CanvasWidth convas.CanvasHeight

    let wavHeader = WavAudio.readHeader fileName false
    let wavData = WavAudio.readData fileName wavHeader
    let sampleInfo = WavAudio.getSampleInfo wavHeader
    let fftBlockSize = pown 2 12

    let fftUserStateAggregator oldData newData =
        match newData with
        | Some data -> Array.append oldData data 
        | None -> oldData

    let fftAnimator canvas frameState = 

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
                |> WaveformViz.drawWaveformYOffset (canvas |> Drawille.clear) (int canvas.Height - 1)
                |> ConViz.updateConsole

                processBlock next

        processBlock frameState.UserState

    let viz = Vizualizer((fftAnimator canvas), fftUserStateAggregator, Array.zeroCreate 0).Start
    
    let avgBytesPerSecond = (wavHeader.SampleRate * wavHeader.BitsPerSample) / 8
    let avgBytesPerMs = avgBytesPerSecond / 1000
    let bufferSizeMs = 100
    let bytesPerLoop = avgBytesPerMs * bufferSizeMs
    let bytesPerLoopPadded = 
        Seq.initInfinite (fun i -> pown 2 i)
        |> Seq.takeWhile (fun i -> i / 2 < bytesPerLoop)
        |> Seq.last
    let mutable i = 0
    let throttleResolution = 10
    let avgBytesPerCheck = avgBytesPerSecond / throttleResolution

    let timer = System.Diagnostics.Stopwatch()
    timer.Start()

    while i * bytesPerLoopPadded < Array.length wavData do

        let readOffset = i * bytesPerLoopPadded
        let byteCount = min bytesPerLoopPadded (Array.length wavData - readOffset)
        let sampleBytes = Array.sub wavData readOffset byteCount
        viz.Post <| sampleBytes
        
        let expectedBytes = timer.Elapsed.TotalMilliseconds * float bytesPerLoopPadded |> int
        let diff = readOffset - expectedBytes
        // if diff > 0 then
        //     let sleepMs = diff / avgBytesPerMs
        //     System.Diagnostics.Debug.WriteLine(sprintf "Sleeping for %i ms" sleepMs)
        //     Threading.Thread.Sleep (sleepMs)

        Threading.Thread.Sleep (bufferSizeMs / 2)

        i <- i + 1

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
    let fileName = @"C:\Dev\MarkDrop\Audio\kicks-sparse.wav"

    if argv.[0] = "-w" then

        drawwaveform fileName

    else if argv.[0] = "-v" then

        //SimpleViz.drawRawFFT fileName
        asyncFFT fileName

    0