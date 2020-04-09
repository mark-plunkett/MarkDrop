
#load "Util.fs"
#load "Converters.fs"
#load "WavAudio.fs"
#load "Drawille.fs"
#load "ConViz.fs"

open Drawille
open ConViz
open WavAudio

//// [-100..101]
//// |> Util.furthestFromZero 0

//ConViz.drawRect 10 8
//|> printfn "%s"


//let tcanvas = Drawille.createPixelCanvas 200 8
//let temps = Array2D.create 2 210 0

//ConViz.traceWaveform tcanvas temps
//    |> Drawille.toStrings
//    |> Seq.reduce (+)
//    |> printfn "%s"
