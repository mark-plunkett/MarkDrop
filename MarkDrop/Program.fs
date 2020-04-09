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

    let w = Console.WindowWidth
    let h = Console.WindowHeight
    
    let canvasWidth = (w * 2) - 2
    let canvasHeight = (h * 4) - 8
    let canvas = Drawille.createPixelCanvas canvasWidth canvasHeight
    let sampleChunkSize = ConViz.getChunkSize wavHeader canvas
    let scalingFactor = (pown 2 wavHeader.BitsPerSample) / canvas.Height
    let offset = int canvas.Height / 2

    WavAudio.processData fileName wavHeader sampleChunkSize
        |> Seq.mapi (fun i s -> 
            s
            |> ConViz.averageChannels
            |> List.map (fun amplitude -> Drawille.pixel i (amplitude |> ConViz.normalize scalingFactor offset))
        )
        |> Seq.collect id
        |> Seq.pairwise
        |> Seq.fold (fun c (p1, p2) -> Drawille.drawLine p1 p2 c) canvas
        |> Drawille.toStrings
        |> List.ofSeq
        |> Seq.reduce (+)
        |> printfn "%s"

   

    0