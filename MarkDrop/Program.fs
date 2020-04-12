open System

[<EntryPoint>]
let main argv =

    // resources
    // WAV format: https://web.archive.org/web/20141213140451/https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
    // FFT: https://en.wikipedia.org/wiki/Fast_Fourier_transform
    // Cooley–Tukey FFT: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

    // !!! need to set this for unicode in Powershell 
    // $OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding

    let fileName = @"C:\Dev\MarkDrop\Audio\single-sine.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\test-phased.wav"
    //let fileName = @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav"
        
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
    let convas = {
        ConViz.Convas.ScalingFactor = (pown 2 wavHeader.BitsPerSample) / int canvas.Height
        ConViz.Convas.ZeroOffset = int canvas.Height / 2
    }
    let samplesPerChunk = ConViz.getChunkSize wavHeader canvas
    
    let printNaieveAverage fileName =
        WavAudio.processAllData fileName samplesPerChunk
        |> ConViz.naieveAverage convas canvas
        |> ConViz.updateConsole
        
    let printMinMaxParallel fileName =
        WavAudio.parallelMapAllData fileName samplesPerChunk ConViz.minMaxValues2D
        |> Seq.map (fun (x, (min, max)) -> ConViz.minMaxToPixels convas x min max)
        |> Seq.fold ConViz.pointFolder canvas

    let printMinMaxParallelMapped fileName =
        WavAudio.parallelMapAllData fileName samplesPerChunk ConViz.minMaxValues2D
        |> Seq.map (fun (x, (min, max)) -> ConViz.minMaxToPixels convas x min max)
        |> Seq.fold ConViz.pointFolder canvas

    let printMinMaxParallel fileName =
        WavAudio.parallelProcessAllData fileName samplesPerChunk
        |> ConViz.parallelMinMax convas canvas

    let printDirect fileName = 
        WavAudio.processData fileName samplesPerChunk
        |> Seq.mapi (fun i s -> 
            s
            |> ConViz.averageChannels
            |> List.map (fun amplitude -> Drawille.pixel i (amplitude |> ConViz.normalize convas))
        )

    printMinMaxParallel fileName
    |> ignore

    Console.CursorTop <- cursorEndY
    0