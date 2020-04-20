
#r @"C:\Users\markj\.nuget\packages\fsharp.collections.parallelseq\1.1.2\lib\netstandard2.0\FSharp.Collections.ParallelSeq.dll"

open FSharp.Collections

#load "Util.fs"
#load "Converters.fs"
#load "WavAudio.fs"
#load "Drawille.fs"
#load "ConViz.fs"
#load "EQViz.fs"
#load "FFT.fs"
#load "WaveformViz.fs"

open Drawille
open ConViz
open WavAudio
open EQViz

//// [-100..101]
//// |> Util.furthestFromZero 0

//createPixelCanvas 100 80
//|> Drawille.drawRect 20 16
//|> Drawille.toStrings
//|> Seq.reduce (+)
//|> printfn "%s"

// TODO: proper fps/tick management - https://gamedev.stackexchange.com/questions/49591/extremely-confused-over-constant-game-speed-maximum-fps-game-loop

System.Console.CursorVisible <- false
System.Console.OutputEncoding <- System.Text.Encoding.UTF8

let r = new System.Random()

let fps = 30
let msPerFrame = 1000 / fps
let timer = new System.Timers.Timer()
timer.Start()
let canvasI = createPixelCanvas 256 160
let canvasO = createOffsetPixelCanvas 256 160 0 160

//while true do

//    Drawille.clear canvas

//    let bands = 
//        EQViz.bands
//        |> List.mapi (fun i b -> (i, r.Next(10, 80)))
//        |> Map.ofList

//    canvas
//    |> EQViz.drawEQ bands
//    |> ConViz.updateConsole 
    
//    System.Threading.Thread.Sleep(msPerFrame)

/// !!! read this https://stackoverflow.com/questions/6740545/understanding-fft-output
  
let max = pown 2 8
let nexti i =
    float i % float max

let mutable i = 1.
while true do

    let div = nexti i
    let input = [for x in 0. .. float max - 1. -> sin(x/i)]
    i <- i + 1.

    let output = FFT.fftList input   

    Drawille.clear canvasI
    input
    |> WaveformViz.drawWaveform canvasI
    |> ConViz.updateConsole 

    Drawille.clear canvasO
    output
    |> WaveformViz.drawWaveform canvasO
    //|> ignore
    |> ConViz.updateConsole 
        
    System.Threading.Thread.Sleep(msPerFrame)

