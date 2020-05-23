module SimpleViz

    open System

    type StreamState = {
        ReadOffset: int
        SamplesProcessed: int
        SampleProcessingRate: int
        Skips: int
    }

    //let drawRawFFT fileName =

    //    Console.OutputEncoding <- Text.Encoding.UTF8
    //    Console.CursorVisible <- false
    //    let originalCursorTop = Console.CursorTop
    //    let w = 128 //Console.WindowWidth
    //    let h = Console.WindowHeight
    //    let cursorEndY = originalCursorTop + (h / 2)
    
    //    let canvasWidth = (w * 2)
    //    let canvasHeight = (h * 2) - 8
    
    //    let canvas = Drawille.createPixelCanvas canvasWidth canvasHeight
    
    //    let wavHeader = WavAudio.readHeader fileName false
    //    let wavData = WavAudio.readData fileName wavHeader
    //    let sampleInfo = WavAudio.getSampleInfo wavHeader
    
    //    let targetFps = 100.
    //    let msPerFrame = 1000./targetFps
    //    let samplesPerLoopRaw = sampleInfo.SampleRate / int targetFps
    //    let samplesPerLoopPadded = 
    //        Seq.initInfinite (fun i -> pown 2 i)
    //        |> Seq.takeWhile (fun i -> i / 2 < samplesPerLoopRaw)
    //        |> Seq.last

    //    let bytesPerLoopRaw = samplesPerLoopRaw * sampleInfo.BytesPerSample
    //    let bytesPerLoopPadded = samplesPerLoopPadded * sampleInfo.BytesPerSample
    //    let fPerPixel = (float sampleInfo.SampleRate / 2.) / float w
    //    let fString = 
    //        [0..w]
    //        |> List.where (fun i -> i % 10 = 0)
    //        |> List.map (fun i -> float i * fPerPixel |> int |> string)
    //        |> List.map (fun s -> s.PadRight(10))
    //        |> List.reduce (+)
    
    //    let initialState = {
    //        ReadOffset = 0
    //        SamplesProcessed = 0
    //        SampleProcessingRate = 0
    //        Skips = 0
    //    }
    
    //    let printDebugInfo (frameState: ConViz.FrameState) streamState =

    //        let time = TimeSpan.FromMilliseconds(float frameState.TotalMs).ToString()
    //        ConViz.updateConsolePos 0 1 
    //            (sprintf 
    //                "bytes processed: %i    samples processed: %i    processing rate (samples/sec): %i   time: %s    skips: %i" 
    //                streamState.ReadOffset 
    //                streamState.SamplesProcessed
    //                streamState.SampleProcessingRate
    //                time
    //                streamState.Skips
    //                )

    //    let mutable zeroedBytes = Array.zeroCreate bytesPerLoopPadded
    //    let fftViz (frameState: ConViz.FrameState) canvas streamState =

    //        if Array.length wavData < (streamState.ReadOffset + bytesPerLoopRaw) then
    //            None
    //        //else if frameState.FrameDurationMs > int64 msPerFrame then
    //        //    // Skip this iteration if we are lagging
    //        //    let nextState = { 
    //        //        streamState with 
    //        //            ReadOffset = streamState.ReadOffset + bytesPerLoopRaw
    //        //            SamplesProcessed = streamState.SamplesProcessed + samplesPerLoopRaw
    //        //            Skips = streamState.Skips + 1
    //        //            }
    //        //    Some (canvas, nextState)
    //        else

    //            //let sleepMs = msPerFrame - float frameState.FrameDurationMs
    //            //if sleepMs > 0. then
    //            //    Threading.Thread.Sleep(int sleepMs)

            
    //            let sampleBytes = 
    //                Array.sub wavData streamState.ReadOffset bytesPerLoopRaw
    //            Array.blit sampleBytes 0 zeroedBytes 0 bytesPerLoopRaw
    //            let samples = WavAudio.bytesToSamples sampleInfo zeroedBytes

    //            // TODO: this is only using left channel atm
    //            let output = 
    //                FFT.fftList (samples.[0,*] |> Array.map float |> List.ofArray) 
    //                |> List.map abs
    
    //            output
    //            |> WaveformViz.drawWaveformYOffset (canvas |> Drawille.clear) (canvasHeight - 1)
    //            |> ConViz.updateConsole

    //            printDebugInfo frameState streamState

    //            let samplesProcessed = streamState.SamplesProcessed + Array2D.length2 samples
    //            let rate = if frameState.TotalMs > 0L then float samplesProcessed / float frameState.TotalMs else 0.
    //            let nextState = { 
    //                streamState with 
    //                    ReadOffset = streamState.ReadOffset + bytesPerLoopRaw
    //                    SamplesProcessed = samplesProcessed 
    //                    SampleProcessingRate = int (rate * 1000.)
    //                    }

    //            Some (canvas, nextState)

    //    ConViz.updateConsolePos 0 (h/2) fString
    //    ConViz.animateState fftViz canvas initialState

    //    Console.CursorTop <- cursorEndY