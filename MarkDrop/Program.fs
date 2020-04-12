open System
open System.Drawing

[<EntryPoint>]
let main argv =

    // resources
    // WAV format: https://web.archive.org/web/20141213140451/https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
    // FFT: https://en.wikipedia.org/wiki/Fast_Fourier_transform
    // Cooley–Tukey FFT: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

    // !!! need to set this for unicode in Powershell 
    // $OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding

    //let fileName = @"C:\Dev\MarkDrop\Audio\single-sine.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\test-phased.wav"
    let fileName = @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav"
        
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
    let samplesPerChunk = ConViz.getChunkSize wavHeader canvas
    let scalingFactor = (pown 2 wavHeader.BitsPerSample) / canvas.Height
    let offset = int canvas.Height / 2
    
    let printNaieveAverage fileName =
        WavAudio.processAllData fileName samplesPerChunk
        |> ConViz.naieveAverage canvas scalingFactor offset
        |> ConViz.updateConsole
        
    let printMinMax fileName =
        WavAudio.processAllData fileName samplesPerChunk
        |> ConViz.minMax canvas scalingFactor offset
        |> ConViz.updateConsole

    let printMinMaxParallel fileName =
        WavAudio.parallelProcessAllData fileName samplesPerChunk
        |> ConViz.parallelMinMax canvas scalingFactor offset

    let printDirect fileName = 
        WavAudio.processData fileName samplesPerChunk
        |> Seq.mapi (fun i s -> 
            s
            |> ConViz.averageChannels
            |> List.map (fun amplitude -> Drawille.pixel i (amplitude |> ConViz.normalize scalingFactor offset))
        )

    printMinMaxParallel fileName
    |> ignore

    Console.CursorTop <- cursorEndY
    0