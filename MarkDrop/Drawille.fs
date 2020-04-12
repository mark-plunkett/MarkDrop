module Drawille

    open Util

    let dotMap = [
        [0x01; 0x08]
        [0x02; 0x10]
        [0x04; 0x20]
        [0x40; 0x80]
    ]

    let brailleCharOffset = 0x2800

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

    //type Canvas = {
    //    Grid: Map<BrailleCharPosition, BrailleChar>
    //    Width: int<PixelX>
    //    Height: int<PixelY>
    //}

    type Canvas = {
        Grid: int[,]
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
        Grid = Array2D.zeroCreate w h
        Width = LanguagePrimitives.Int32WithMeasure w
        Height = LanguagePrimitives.Int32WithMeasure h
    }

    let getMappedBrailleChar pixel =
        // Ignore the units here for the purposes of getting the mapped braille dot
        dotMap.[(int pixel.Y % 4)].[(int pixel.X % 2)]

    let brailleToString i =
        char (brailleCharOffset + i) |> string

    let bitMask brailleCharacter mask bitwiseOp =
        bitwiseOp brailleCharacter mask

    let modify pixel canvas bitwiseOp =
        let brailleCharPosition = pixelToBraillePosition pixel
        let existingValue = Array2D.get canvas.Grid (int brailleCharPosition.X) (int brailleCharPosition.Y)
        let dotVal = getMappedBrailleChar pixel
        if existingValue ||| dotVal = existingValue then
            canvas
        else
            let masked = bitMask existingValue dotVal bitwiseOp
            Array2D.set canvas.Grid (int brailleCharPosition.X) (int brailleCharPosition.Y) masked
            canvas

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

    let enumerate canvas f =
        let mutable x = 0
        let mutable y = 0
        while y < Array2D.length1 canvas.Grid do
            while x < Array2D.length2 canvas.Grid do
                f (x, y) canvas.Grid.[y, x]

                x <- x + 1

            y <- y + 1

    let enumerate2 (g1: int[,]) (g2: int[,]) f =
        let mutable x = 0
        while x < Array2D.length1 g1 do
            let mutable y = 0
            while y < Array2D.length2 g1 do
                f (x, y) g1.[x, y] g2.[x, y]

                y <- y + 1
            x <- x + 1

    let multAndRound a b =
        let dec = float a * float b
        int (round dec)
    
    let drawLine fromPixel toPixel canvas =
        //printfn "drawing line from %i,%i to %i,%i" fromPixel.X fromPixel.Y toPixel.X toPixel.Y
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
                    match Array2D.get canvas.Grid x y with
                    | 0 -> yield " "
                    | braille -> yield brailleToString braille

                yield "\n"
        }
