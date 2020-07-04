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
#load "Animation.fs"

open Drawille
open WavAudio

try
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    System.Console.CursorVisible <- false
with
    _ -> ()

let canvasWidth = 360

let canvasI = createPixelCanvas canvasWidth 160
let pi = System.Math.PI
let time = 1.
let numSamplesI = pown 2 10
let numSamplesF = float numSamplesI
let samplingFrequency = numSamplesF/time
let deltaT = time/samplingFrequency

let sampleInfo = {
    BytesPerSample = 2
    NumChannels= 2
    BytesPerMultiChannelSample= 2 * 2 
    SampleRate= 44100
    BitDepth = pown 2 16
}

let numSamples = pown 2 8
let numBytes = numSamples * sampleInfo.BytesPerMultiChannelSample
let sampleArray = [|0..numSamples|]
let bitDepth = float sampleInfo.BitDepth

let toSineWave frequency amplitude t =
    (amplitude * sin(2. * pi * (bitDepth / frequency) * t))

let toSineWaves i t =
    (toSineWave i 1. t) //+ (toSineWave (100. + i) 1. t) //+ (toSineWave 1000. 1. t)

let generateSamples amplitude frequency offset =
    sampleArray
    |> Array.map (fun i -> 
        let i' = i * offset |> float
        float sampleInfo.BitDepth * toSineWave frequency amplitude i' |> int16 |> Convert.int16ToBytes
    )

let viz = Animation.Phase.phaseRaw sampleInfo
let a = viz.Start
use stream = new System.IO.MemoryStream(numBytes)
Animation.animate sampleInfo stream viz.Start true
let rec postSamples offset =

    let lSamples = generateSamples 1. 440. offset
    let rSamples = generateSamples 0.5 440. offset
    let samples = Array.map2 (fun l r -> [|l;r|]) lSamples rSamples |> Array.concat |> Array.collect id

    stream.Write(samples, 0, numSamples)

    postSamples (offset + numSamples)

postSamples 0 |> ignore