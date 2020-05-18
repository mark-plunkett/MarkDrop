open System

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
    |> Seq.reduce (+)
    |> printfn "%s"
    
    Console.CursorTop <- cursorEndY

type StreamState = {
    ReadOffset: int
    SamplesProcessed: int
    SampleProcessingRate: int
    Skips: int
}

let drawFFT fileName =

    Console.OutputEncoding <- Text.Encoding.UTF8
    Console.CursorVisible <- false
    let originalCursorTop = Console.CursorTop
    let w = 128 //Console.WindowWidth
    let h = Console.WindowHeight
    let cursorEndY = originalCursorTop + (h / 2)
    
    let canvasWidth = (w * 2)
    let canvasHeight = (h * 2) - 8
    
    let canvas = Drawille.createPixelCanvas canvasWidth canvasHeight
    
    let wavHeader = WavAudio.readHeader fileName false
    let wavData = WavAudio.readData fileName wavHeader
    let sampleInfo = WavAudio.getSampleInfo wavHeader
    
    let targetFps = 30.
    let msPerFrame = 1000./targetFps
    let samplesPerLoopRaw = sampleInfo.SampleRate / int targetFps
    let samplesPerLoopPadded = 
        Seq.initInfinite (fun i -> pown 2 i)
        |> Seq.takeWhile (fun i -> i / 2 < samplesPerLoopRaw)
        |> Seq.last

    let bytesPerLoopRaw = samplesPerLoopRaw * sampleInfo.BytesPerSample
    let bytesPerLoopPadded = samplesPerLoopPadded * sampleInfo.BytesPerSample
    let fPerPixel = (float sampleInfo.SampleRate / 2.) / float w
    let fString = 
        [0..w]
        |> List.where (fun i -> i % 10 = 0)
        |> List.map (fun i -> float i * fPerPixel |> int |> string)
        |> List.map (fun s -> s.PadRight(10))
        |> List.reduce (+)
    
    let initialState = {
        ReadOffset = 0
        SamplesProcessed = 0
        SampleProcessingRate = 0
        Skips = 0
    }
    
    let printDebugInfo (frameState: ConViz.FrameState) streamState =

        let time = TimeSpan.FromMilliseconds(float frameState.TotalMs).ToString()
        ConViz.updateConsolePos 0 1 
            (sprintf 
                "bytes processed: %i    samples processed: %i    processing rate (samples/sec): %i   time: %s    skips: %i" 
                streamState.ReadOffset 
                streamState.SamplesProcessed
                streamState.SampleProcessingRate
                time
                streamState.Skips
                )

    let mutable zeroedBytes = Array.zeroCreate bytesPerLoopPadded
    let fftViz (frameState: ConViz.FrameState) canvas streamState =

        if Array.length wavData < (streamState.ReadOffset + bytesPerLoopRaw) then
            None
        //else if frameState.FrameDurationMs > int64 msPerFrame then
        //    // Skip this iteration if we are lagging
        //    let nextState = { 
        //        streamState with 
        //            ReadOffset = streamState.ReadOffset + bytesPerLoopRaw
        //            SamplesProcessed = streamState.SamplesProcessed + samplesPerLoopRaw
        //            Skips = streamState.Skips + 1
        //            }
        //    Some (canvas, nextState)
        else

            let sleepMs = msPerFrame - float frameState.FrameDurationMs
            if sleepMs > 0. then
                Threading.Thread.Sleep(int sleepMs)

            
            let sampleBytes = 
                Array.sub wavData streamState.ReadOffset bytesPerLoopRaw
            Array.blit sampleBytes 0 zeroedBytes 0 bytesPerLoopRaw
            let samples = WavAudio.bytesToSamples sampleInfo zeroedBytes

            // TODO: this is only using left channel atm
            let output = 
                FFT.fftList (samples.[0,*] |> Array.map float |> List.ofArray) 
                |> List.map abs
    
            output
            |> WaveformViz.drawWaveformYOffset (canvas |> Drawille.clear) (canvasHeight - 1)
            |> ConViz.updateConsole

            printDebugInfo frameState streamState

            let samplesProcessed = streamState.SamplesProcessed + Array2D.length2 samples
            let rate = if frameState.TotalMs > 0L then float samplesProcessed / float frameState.TotalMs else 0.
            let nextState = { 
                streamState with 
                    ReadOffset = streamState.ReadOffset + bytesPerLoopRaw
                    SamplesProcessed = samplesProcessed 
                    SampleProcessingRate = int (rate * 1000.)
                    }

            Some (canvas, nextState)

    ConViz.updateConsolePos 0 (h/2) fString
    ConViz.animateState fftViz canvas initialState

    Console.CursorTop <- cursorEndY

[<EntryPoint>]
let main argv =

    // resources
    // WAV format: https://web.archive.org/web/20141213140451/https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
    // FFT: https://en.wikipedia.org/wiki/Fast_Fourier_transform
    // Cooley–Tukey FFT: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

    // !!! need to set this for unicode in Powershell 
    // $OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding

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

        drawFFT fileName

    0