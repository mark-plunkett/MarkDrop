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
open Drawille.Shapes

(*
    Need to read audio stream and process it at a rate defined by SampleInfo.SampleRate
    Process will be 
        
        read x samples, where x is some power of 2
        
        do fft

        update vizualization

*)

try
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    System.Console.CursorVisible <- false
with
    _ -> ()

let canvasWidth = 360

let canvas = createPixelCanvas canvasWidth 160

let trig length trigFunc angle =
    int (length * (trigFunc angle))

let rotate angleRadians pixel =
    let length = sqrt ((float pixel.X**2.) + (float pixel.Y**2.))
    Drawille.pixel 
        ((trig (pixel.X|>int|>float) cos angleRadians) - (trig (pixel.Y|>int|>float) sin angleRadians)) 
        ((trig (pixel.X|>int|>float) sin angleRadians) + (trig (pixel.Y|>int|>float) cos angleRadians)) 

let translate origin p =
    pixel (int p.X + int origin.X) (int p.Y + int origin.Y)

let slowFill (state: ConViz.FrameState) canvas = 
    // fills with vertical lines, 1 per frame
    let x = state.FrameCount % int canvas.Width
    drawLine (pixel x 0) (pixel x ((int canvas.Height) - 1)) canvas

let rotateRect (state: ConViz.FrameState) canvas =
    // draws and rotates a square in centre of canvas
    let aRadians = ((2. * System.Math.PI) / 360.) * float state.FrameCount
    let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
    let rectDimensions = {| Width = 40; Height = 40 |}
    let rectOffsets = {| XOffset = rectDimensions.Width/2; YOffset = rectDimensions.Height/2 |}

    let rect = {
        A = pixel (-rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
        B = pixel (+rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
        C = pixel (+rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
        D = pixel (-rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
    }

    canvas
    |> clear
    |> drawRect rect

let rotateLine (state: ConViz.FrameState) canvas =
    
    let aRadians = ((2. * System.Math.PI) / 360.) * float state.FrameCount
    let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
    let length = 50.    
    let pos = pixel (int length) 0 |> rotate aRadians |> translate origin
    canvas |> clear |> Drawille.drawLine origin pos


ConViz.animate rotateRect canvas