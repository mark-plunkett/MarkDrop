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
    // !!! 24BIT IS BROKEN
    //let fileName = @"D:\Google Drive\Production\Samples\# Synth Drums\unprocessed drums\toms\unusual toms\wasd_tom_sys100_ceramic-2_s_u.wav"
    // LONG FILE ~1GB
    // let fileName = @"D:\Google Drive\Music\Mixes\Jungle\Gold Dubs Revamped Classics Mix 2014.wav"
    let fileName = if argv.Length = 0 then  @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav" else argv.[0]
        
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

    0