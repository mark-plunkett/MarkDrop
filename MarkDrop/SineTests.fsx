#r @"C:\Users\markj\.nuget\packages\fsharpx.collections\2.1.2\lib\netstandard2.0\FSharpx.Collections.dll"
#r @"C:\Users\markj\.nuget\packages\fsharp.collections.parallelseq\1.1.2\lib\netstandard2.0\FSharp.Collections.ParallelSeq.dll"

#load "Converters.fs"
#load "Util.fs"
#load "FFT.fs"
#load "Drawille.fs"
#load "WavAudio.fs"
#load "ConViz.fs"
#load "WaveformViz.fs"

open FSharpx.Collections
open Util
open Drawille

// Generate 3 sine waves over 44100 samples
// - 100 - 0.5
// - 600 - 1
// - 10000 - 0.75

let canvasWidth = 360

let canvasI = createPixelCanvas canvasWidth 160
let pi = System.Math.PI
let time = 1.
let numSamples = 16384.
let samplingFrequency = numSamples/time
let deltaT = time/samplingFrequency

let toSineWaves t =
    (0.75 * sin(2.*pi*100.*t)) + (1. * sin(2.*pi*600.*t)) + (0.75 * sin(2.*pi*1000.*t))

let samples = [for t in 0. ..numSamples-1. -> toSineWaves (t*deltaT)]

samples
|> List.sort
|> List.take 100
|> printfn "%A"

samples
|> WaveformViz.drawWaveform canvasI
|> ConViz.drawCanvas

// Do FFT
let canvasO = createOffsetPixelCanvas canvasWidth 160 0 160
let output = FFT.fftList samples

output
|> WaveformViz.drawWaveform canvasO
|> ConViz.drawCanvas

let binWidth = samplingFrequency / numSamples
let fMax = List.length output / 2
let space = canvasWidth / 10
let frequenciesPerChar = output.Length / (canvasWidth / 2)
let frequencies = 
    [for f in 0..fMax -> f * frequenciesPerChar]
    |> List.filter (fun f -> f % space = 0)
    |> List.map string
    |> List.map (fun f -> f.PadRight(10, ' '))
    |> List.reduce (+)

printfn "%s" frequencies