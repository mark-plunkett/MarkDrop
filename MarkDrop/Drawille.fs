module Drawille

    open Util

    let dotMap = [
        [0x01; 0x08]
        [0x02; 0x10]
        [0x04; 0x20]
        [0x40; 0x80]
    ]

    let brailleCharOffset = 0x2800

    type BrailleChar = BrailleChar of int

    type Point = {
        X: int
        Y: int
    }

    type Canvas = {
        Grid: Map<Point, BrailleChar>
        Width: int
        Height: int
    }

    let point x y = {
        X = x
        Y = y
    }

    let toBrailleCoords x y = 
        point (x / 2) (y / 4)

    let createCanvasAbsolute w h = 
        {
            Grid = Map.empty
            Width = w
            Height = h
        }

    let createCanvas w h =
        let point = toBrailleCoords w h
        createCanvasAbsolute point.X point.Y

    let getMappedBrailleChar x y =
        BrailleChar dotMap.[(y % 4)].[(x % 2)]

    let brailleToString i =
        let (BrailleChar braille) = i
        char (brailleCharOffset + int braille) |> string

    let bitMask brailleCharacter mask bitwiseOp =
        let (BrailleChar brailleCharacterInt) = brailleCharacter
        let (BrailleChar maskInt) = mask
        BrailleChar (bitwiseOp brailleCharacterInt maskInt)

    let modify x y canvas bitwiseOp =
        let point = toBrailleCoords x y
        let dotVal = getMappedBrailleChar x y
        match canvas.Grid |> Map.tryFind point with
            | None -> 
                { canvas with Grid = canvas.Grid |> Map.add point dotVal }
            | Some braille ->
                let masked = bitMask braille dotVal bitwiseOp
                let grid = canvas.Grid |> Map.remove point |> Map.add point masked
                { canvas with Grid = grid }

    let set x y canvas =
        modify x y canvas (|||)

    let unset x y canvas =
        modify x y canvas (^^^)

    let toggle x y canvas =
        let dotVal = getMappedBrailleChar x y
        // TODO: try and call modify with xor dotVal since this will toggle the target dot
        ()

    let rec draw points canvas =
        match points with
        | head :: tail -> 
            draw tail (set head.X head.Y canvas)
        | [] -> canvas

    //let turtle fromPoint toPoint canvas =
    //    // e.g. 0,0 to 20,15
    //    let xDelta = toPoint.X - fromPoint.X
    //    let yDelta = toPoint.Y - fromPoint.Y
    //    let grad =
    //        match xDelta with
    //        | 0 -> decimal (canvas.Width - toPoint.X)
    //        | _ -> decimal yDelta / decimal xDelta

    //    let xStep = 
    //        match grad with
    //        | 0.0m -> 0.0m
    //        | _ -> abs (decimal xDelta / decimal grad)
    //    let yStep = 
    //        match grad with
    //        | 0.0m -> 0.0m
    //        | _ -> decimal yDelta / decimal grad

    //    [0..max (abs xDelta) (abs yDelta)]
    //    |> List.map (fun i -> point (fromPoint.X + i) (fromPoint.Y + int (decimal i * yStep)))
    //    //|> Util.iterTrans (fun p -> printfn "%i:%i" p.X p.Y)
    //    |> flip draw canvas
    
    let multAndRound a b =
        let dec = float a * float b
        int (round dec)
    
    let drawLine fromPoint toPoint canvas =
        let xDelta = toPoint.X - fromPoint.X
        let yDelta = toPoint.Y - fromPoint.Y
        let max = max (abs xDelta) (abs yDelta)
        let xStep = float xDelta / float max
        let yStep = float yDelta / float max
        [0..max-1]
        |> List.map (fun i -> point (fromPoint.X + multAndRound i xStep) (fromPoint.Y + multAndRound i yStep))
        //|> Util.iterTrans (fun p -> printfn "%i:%i" p.X p.Y)
        |> flip draw canvas

    let toStrings canvas =
        let brailleCanvas = toBrailleCoords canvas.Width canvas.Height
        seq {
            for y in [0..brailleCanvas.Y-1] do
                for x in [0..brailleCanvas.X-1] do
                    match canvas.Grid |> Map.tryFind { X = x; Y = y } with
                    | Some braille -> yield brailleToString braille
                    | None -> yield " "

                yield "\n"
        }
