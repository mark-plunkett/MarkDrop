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

    