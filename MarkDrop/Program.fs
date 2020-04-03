// Learn more about F# at http://fsharp.org

open FSharp.Charting

open System
open System.Drawing

let extractSamples f samples = 
    seq {
        for s in samples do
            yield f s
    }

let displayWaveformChart granularity (samples:seq<WavAudio.StereoSample>) =
    
    let leftSamples = 
        samples
        |> Seq.map (fun s -> decimal s.AmpL)
        |> Seq.chunkBySize granularity
        |> Seq.map (fun s -> Array.average s)
        |> Seq.toArray

    let rightSamples = 
        samples 
        |> Seq.map (fun s -> decimal (-s.AmpR))
        |> Seq.chunkBySize granularity
        |> Seq.map (fun s -> Array.average s)
        |> Seq.toArray
        
    Chart.Combine([
        Chart.Line(leftSamples)
        Chart.Line(rightSamples)])
    |> Chart.Show



[<EntryPoint>]
let main argv =

    // resources
    // WAV format: https://web.archive.org/web/20141213140451/https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
    // FFT: https://en.wikipedia.org/wiki/Fast_Fourier_transform
    // Cooley–Tukey FFT: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

    let fileName = @"C:\Dev\MarkDrop\Audio\test-phased.wav"
        
    let wavHeader = WavAudio.readHeader fileName false

    wavHeader |> WavAudio.printInfo fileName

    let w = Console.WindowWidth
    let h = Console.WindowHeight

    WavAudio.streamData fileName wavHeader 
    |> Seq.toList
    |> ConViz.traceWaveForm w h
    |> printfn "%s"
    
    //ConViz.drawRect 50 50
    //|> printfn "%s" 

    // !!! need to set this for unicode in Powershell 
    // $OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding
       
    0
