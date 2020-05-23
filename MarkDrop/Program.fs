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

let asyncFFT fileName =

    (*
        Setup async animation loop that processes data according to the sample rate of the source.
        Data will be provided in chunks of some size which is a multiple of BlockAlign (usually 4)
    
        Data should be concat'ed and provided to a processor, which can
            - optionally choose to process the data e.g. if enough data is available
            - return a new state, minus the processed data
    *)
    
    let wavHeader = WavAudio.readHeader fileName false
    let wavData = WavAudio.readData fileName wavHeader
    let sampleInfo = WavAudio.getSampleInfo wavHeader

    let rectAnimator canvas state =
        ConViz.rotateRect canvas state
        |> ConViz.updateConsole

    let rectUserStateAggregator oldData newData =
        match newData with
        | Some data -> data
        | None -> oldData

    //let fftAnimator state msg = 
    //    let samples = WavAudio.bytesToSamples sampleInfo msg.Bytes
    
    //    // TODO: this is only using left channel atm
    //    let output = 
    //        FFT.fftList (samples.[0,*] |> Array.map float |> List.ofArray) 
    //        |> List.map abs
        
    //    output
    //    |> WaveformViz.drawWaveformYOffset (canvas |> Drawille.clear) (canvasHeight - 1)
    //    |> ConViz.updateConsole


    let convas = ConViz.initialise
    let canvas = Drawille.createPixelCanvas convas.CanvasWidth convas.CanvasHeight
    let viz = Vizualizer((rectAnimator canvas), rectUserStateAggregator, 1.).Start

    let rec feed f =

        viz.Post f 
        Threading.Thread.Sleep 500
        feed <| f + 1.

    feed 1. |> ignore


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

    if argv.[0] = "-w" then

        drawwaveform fileName

    else if argv.[0] = "-v" then

        //SimpleViz.drawRawFFT fileName
        asyncFFT fileName

    0