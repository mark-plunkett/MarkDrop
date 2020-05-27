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

    type Canvas = {
        Grid: int[,]
        Width: int<PixelX>
        Height: int<PixelY>
    }

    module Shapes =
       
        type Rect = {
            A: Pixel
            B: Pixel
            C: Pixel
            D: Pixel
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

    let createOffsetPixelCanvas w h = 
        let roundedW = ceil (float w / float pixelsPerBrailleX) |> int
        let roundedH = ceil (float h / float pixelsPerBrailleY) |> int
        {
            Grid = Array2D.zeroCreate roundedW roundedH
            Width = LanguagePrimitives.Int32WithMeasure w
            Height = LanguagePrimitives.Int32WithMeasure h
        }

    let createPixelCanvas w h = 
        createOffsetPixelCanvas w h

    let createCharCanvas (w: int) (h: int) =
        createPixelCanvas (w * int pixelsPerBrailleX) (h * int pixelsPerBrailleY)

    let getMappedBrailleChar pixel =
        // Ignore the units here for the purposes of getting the mapped braille dot
        dotMap.[(int pixel.Y % 4)].[(int pixel.X % 2)]

    let brailleToString i =
        char (brailleCharOffset + i) |> string

    let bitMask brailleCharacter mask bitwiseOp =
        bitwiseOp brailleCharacter mask

    let modify pixel canvas bitwiseOp =
        let brailleCharPosition = pixelToBraillePosition pixel
        let x = int brailleCharPosition.X
        let y = int brailleCharPosition.Y
        if x >= Array2D.length1 canvas.Grid 
            || y >= Array2D.length2 canvas.Grid
            || x < 0
            || y < 0 
            || pixel.X >= canvas.Width
            || pixel.Y >= canvas.Height
            || int pixel.X < 0
            || int pixel.Y < 0 then
            canvas
        else
            let existingValue = Array2D.get canvas.Grid x y
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
        modify pixel canvas (fun existing next ->  (existing &&& 0))

    let toggle pixel canvas =
        modify pixel canvas (fun existing next ->  (~~~ existing &&& next))

    let rec draw pixels canvas =
        match pixels with
        | head :: tail -> 
            draw tail (set head canvas)
        | [] -> canvas

    let enumerate canvas f =
        let mutable x = 0
        while x < Array2D.length1 canvas.Grid do
            let mutable y = 0
            while y < Array2D.length2 canvas.Grid do
                f (x, y) (Array2D.get canvas.Grid x y)
                y <- y + 1
            x <- x + 1

    let enumerate2 (g1: int[,]) (g2: int[,]) f =
        let mutable x = 0
        while x < Array2D.length1 g1 do
            let mutable y = 0
            while y < Array2D.length2 g1 do
                f (x, y) (Array2D.get g1 x y) (Array2D.get g2 x y)
                y <- y + 1
            x <- x + 1

    let clear canvas =
        enumerate canvas (fun (x, y) _ -> Array2D.set canvas.Grid x y 0)
        canvas

    let modifyPoints f points canvas =
        points 
        |> List.fold (fun c p -> f p c) canvas

    let drawPoints =
        modifyPoints set

    let erasePoints =
        modifyPoints unset

    let togglePoints =
        modifyPoints toggle

    let multAndRound a b =
        let dec = float a * float b
        int (round dec)
    
    let line fromPixel toPixel =
        let xDelta = int toPixel.X - int fromPixel.X
        let yDelta = int toPixel.Y - int fromPixel.Y
        let max = max (abs xDelta) (abs yDelta)
        let xStep = float xDelta / float max
        let yStep = float yDelta / float max
        [0..max]
        |> List.map (fun i -> pixel (int fromPixel.X + multAndRound i xStep) (int fromPixel.Y + multAndRound i yStep))

    let drawLine fromPixel toPixel canvas =
        if toPixel = fromPixel then 
            set fromPixel canvas
        else 
            line fromPixel toPixel
            |> flip drawPoints canvas

    let drawTurtle points canvas =
        points
        |> Seq.pairwise
        |> Seq.fold (fun c (p1, p2) -> drawLine p1 p2 c) canvas
       
    let rect (rect:Shapes.Rect) =
        [
            line rect.A rect.B
            line rect.B rect.C
            line rect.C rect.D
            line rect.D rect.A 
        ]
        |> List.collect id

    let drawRect (rectangle:Shapes.Rect) canvas =
        canvas
        |> drawPoints (rect <| rectangle)

    let fillRect (rect:Shapes.Rect) canvas =
        // TODO: fill the rect...
        canvas
        |> drawLine rect.A rect.B
        |> drawLine rect.B rect.C
        |> drawLine rect.C rect.D
        |> drawLine rect.D rect.A

    let toStrings canvas =
        let mapWidth = canvas.Width / pixelsPerBrailleX
        let mapHeight = canvas.Height / pixelsPerBrailleY
        let builder = System.Text.StringBuilder()

        for y in [0..int mapHeight - 1] do
            for x in [0..int mapWidth - 1] do
                match Array2D.get canvas.Grid x y with
                | 0 -> builder.Append(" ") |> ignore
                | braille -> builder.Append(brailleToString braille) |> ignore

            builder.Append("\n") |> ignore

        builder.ToString()        

    let trig length trigFunc angle =
        int (length * (trigFunc angle))
    
    let rotate angleRadians p =
        pixel 
            ((trig (p.X |> int |> float) cos angleRadians) - (trig (p.Y |> int |> float) sin angleRadians)) 
            ((trig (p.X |> int |> float) sin angleRadians) + (trig (p.Y |> int |> float) cos angleRadians)) 
    
    let translate origin p =
        pixel (int p.X + int origin.X) (int p.Y + int origin.Y)