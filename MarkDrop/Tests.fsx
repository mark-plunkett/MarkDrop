
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
let msPerFrame = 1000 / 30
let timer = new System.Timers.Timer()
timer.Start()
let canvas = createPixelCanvas 100 80

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
    
let convas = WaveformViz.convasFromCanvas canvas 4.
FFT.input
|> WaveformViz.drawWaveform convas canvas
|> Drawille.toStrings
|> Seq.reduce (+)
|> printfn "%s"