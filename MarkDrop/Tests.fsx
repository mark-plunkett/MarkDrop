
#load "Util.fs"
#load "Drawille.fs"

open Drawille

createCanvasAbsolute 100 100
|> drawLine (point 5 0) (point 25 35)
|> drawLine (point 25 35) (point 49 4)
|> drawLine (point 0 35) (point 99 99)
|> toStrings
//|> Util.iterTransSeq (fun p -> printfn "%s" p)
|> Seq.reduce (+)
|> printfn "%s"

 [-100..101]
 |> Util.furthestFromZero 0