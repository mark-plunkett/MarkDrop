#load "../Util.fs"
#load "../Drawille.fs"

open Drawille

let canvas = Drawille.createCharCanvas 1 1

let checkChar c =
    canvas.Grid.[0,0] = c

let cross xs ys =
    seq {
        for x in xs do
            for y in ys do
                yield (x, y)
    }

cross [0..1] [0..4]
|> Seq.map (fun i -> i ||> pixel)
|> Seq.iter (fun p -> Drawille.set p canvas |> ignore)

let allSet = 0xFF

checkChar allSet

Drawille.unset (pixel 0 0) canvas

canvas.Grid

checkChar allSet