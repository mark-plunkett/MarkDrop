// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =

    // resources
    // WAV format: https://web.archive.org/web/20141213140451/https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
    // FFT: https://en.wikipedia.org/wiki/Fast_Fourier_transform
    // Cooley–Tukey FFT: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

    let fileName = @"C:\Dev\MarkDrop\Audio\test.wav"
    let file = System.IO.File.OpenRead(@"C:\Dev\MarkDrop\Audio\test.wav")
    
    let wavFile = WavAudio.readWav file

    wavFile |> WavAudio.printInfo fileName
   
    0
