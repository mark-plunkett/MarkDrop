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

    [<Measure>] type BrailleX
    [<Measure>] type BrailleY

    type BrailleCharPosition = {
        X: int<BrailleX>
        Y: int<BrailleY>
    }

    let braillePosition (x: int) (y: int) = {
        X = LanguagePrimitives.Int32WithMeasure x
        Y = LanguagePrimitives.Int32WithMeasure y
    }

    [<Measure>] type PixelX
    [<Measure>] type PixelY

    type Pixel = {
        X: int<PixelX>
        Y: int<PixelY>
    }

    type Canvas = {
        Grid: Map<BrailleCharPosition, BrailleChar>
        Width: int<PixelX>
        Height: int<PixelY>
    }

    let pixelsPerBrailleX : int<PixelX/BrailleX> = 2<PixelX/BrailleX>
    let pixelsPerBrailleY : int<PixelY/BrailleY> = 4<PixelY/BrailleY>

    let pixelToBraillePosition (p:Pixel) = {
        BrailleCharPosition.X = p.X / pixelsPerBrailleX
        BrailleCharPosition.Y = p.Y / pixelsPerBrailleY
    }

    let pixel (x: int) (y: int) = {
        X = LanguagePrimitives.Int32WithMeasure x
        Y = LanguagePrimitives.Int32WithMeasure y
    }

    let createPixelCanvas w h = {
        Grid = Map.empty
        Width = LanguagePrimitives.Int32WithMeasure w
        Height = LanguagePrimitives.Int32WithMeasure h
    }

    let getMappedBrailleChar pixel =
        // Ignore the units here for the purposes of getting the mapped braille dot
        BrailleChar dotMap.[(int pixel.Y % 4)].[(int pixel.X % 2)]

    let brailleToString i =
        let (BrailleChar braille) = i
        char (brailleCharOffset + int braille) |> string

    let bitMask brailleCharacter mask bitwiseOp =
        let (BrailleChar brailleCharacterInt) = brailleCharacter
        let (BrailleChar maskInt) = mask
        BrailleChar (bitwiseOp brailleCharacterInt maskInt)

    let modify pixel canvas bitwiseOp =
        let dotVal = getMappedBrailleChar pixel
        let brailleCharPosition = pixelToBraillePosition pixel
        match canvas.Grid |> Map.tryFind brailleCharPosition with
            | None -> 
                { canvas with Grid = canvas.Grid |> Map.add brailleCharPosition dotVal }
            | Some braille ->
                let masked = bitMask braille dotVal bitwiseOp
                let grid = canvas.Grid |> Map.remove brailleCharPosition |> Map.add brailleCharPosition masked
                { canvas with Grid = grid }

    let set pixel canvas =
        modify pixel canvas (|||)

    let unset pixel canvas =
        modify pixel canvas (^^^)

    let toggle pixel canvas =
        let dotVal = getMappedBrailleChar pixel
        // TODO: try and call modify with xor dotVal since this will toggle the target dot
        ()

    let rec draw pixels canvas =
        match pixels with
        | head :: tail -> 
            draw tail (set head canvas)
        | [] -> canvas

    let multAndRound a b =
        let dec = float a * float b
        int (round dec)
    
    let drawLine fromPixel toPixel canvas =
        if toPixel = fromPixel then 
            set fromPixel canvas
        else 
            let xDelta = int toPixel.X - int fromPixel.X
            let yDelta = int toPixel.Y - int fromPixel.Y
            let max = max (abs xDelta) (abs yDelta)
            let xStep = float xDelta / float max
            let yStep = float yDelta / float max
            [0..max]
            |> List.map (fun i -> pixel (int fromPixel.X + multAndRound i xStep) (int fromPixel.Y + multAndRound i yStep))
            //|> Util.iterTrans (fun p -> printfn "%i:%i" p.X p.Y)
            |> flip draw canvas

    let toStrings canvas =
        let mapWidth = canvas.Width / pixelsPerBrailleX
        let mapHeight = canvas.Height / pixelsPerBrailleY
        seq {
            for y in [0..int mapHeight - 1] do
                for x in [0..int mapWidth - 1] do
                    match canvas.Grid |> Map.tryFind (braillePosition x y) with
                    | Some braille -> yield brailleToString braille
                    | None -> yield " "

                yield "\n"
        }
