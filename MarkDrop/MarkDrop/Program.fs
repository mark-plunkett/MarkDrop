// Learn more about F# at http://fsharp.org

open System

open WavAudio

[<EntryPoint>]
let main argv =

    let fileName = @"C:\Dev\MarkDrop\Audio\test.wav"
    let file = System.IO.File.OpenRead(@"C:\Dev\MarkDrop\Audio\test.wav")
    
    let wavFile = WavAudio.readWav file

    wavFile |> WavAudio.printInfo fileName
   
    0
