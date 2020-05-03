#r @"C:\Users\markj\.nuget\packages\fsharpx.collections\2.1.2\lib\netstandard2.0\FSharpx.Collections.dll"
#r @"C:\Users\markj\.nuget\packages\fsharp.collections.parallelseq\1.1.2\lib\netstandard2.0\FSharp.Collections.ParallelSeq.dll"

#load "Converters.fs"
#load "Util.fs"
#load "FFT.fs"
#load "FFFT.fs"
#load "Drawille.fs"
#load "WavAudio.fs"
#load "ConViz.fs"
#load "WaveformViz.fs"

open FSharpx.Collections
open Drawille

try
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    System.Console.CursorVisible <- false
with
    _ -> ()

let canvasWidth = 360

let canvasI = createPixelCanvas canvasWidth 160
let canvasO = createOffsetPixelCanvas canvasWidth 160 0 160
let pi = System.Math.PI
let time = 1.
let numSamples = pown 2. 10
let samplingFrequency = numSamples/time
let deltaT = time/samplingFrequency

let toSineWave frequency amplitude t =
    (amplitude * sin(2. * pi * frequency * t))

let toSineWaves i t =
    (toSineWave i 1. t) //+ (toSineWave (100. + i) 1. t) //+ (toSineWave 1000. 1. t)

let fps = 30
let msPerFrame = 1000 / fps
let mutable i = 1.
while true do

    let samples = [for t in 0. ..numSamples-1. -> toSineWaves i (t*deltaT)]

    //samples
    //|> WaveformViz.drawWaveform canvasI
    //|> ConViz.drawCanvas

    // Do FFT
    let output = FFT.fftList samples

    output
    |> WaveformViz.drawWaveform (canvasO |> Drawille.clear)
    |> ConViz.updateConsole

    System.Threading.Thread.Sleep(1)

    i <- i + 1.

    printfn "%f" i

//// Do FFFT
//module FFFFFFFT =
//    let canvasO = createOffsetPixelCanvas canvasWidth 160 0 160
//    let output =
//        samples
//        |> List.map (fun f -> System.Numerics.Complex(f, 0.))
//        |> List.toArray
//        |> FFFT.fft
//        |> Array.map (fun c -> c.Real)
//        |> Array.toList

//    output
//    |> WaveformViz.drawWaveform canvasO
//    |> ConViz.drawCanvas

//    let binWidth = samplingFrequency / numSamples
//    let fMax = List.length output / 2
//    let space = canvasWidth / 10
//    let frequenciesPerChar = output.Length / (canvasWidth / 2)
//    let frequencies = 
//        [for f in 0..fMax -> f * frequenciesPerChar]
//        |> List.filter (fun f -> f % space = 0)
//        |> List.map string
//        |> List.map (fun f -> f.PadRight(10, ' '))
//        |> List.reduce (+)

//    printfn "%s" frequencies

//let binWidth = samplingFrequency / numSamples
//let fMax = List.length output / 2
//let space = canvasWidth / 10
//let frequenciesPerChar = output.Length / (canvasWidth / 2)
//let frequencies = 
//    [for f in 0..fMax -> f * frequenciesPerChar]
//    |> List.filter (fun f -> f % space = 0)
//    |> List.map string
//    |> List.map (fun f -> f.PadRight(10, ' '))
//    |> List.reduce (+)

//printfn "%s" frequencies