module Drawille

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

    let toBrailleCoords x y = { 
        X = (x / 2)
        Y = (y / 4)
    }

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

    let toString canvas =
        let brailleCanvas = toBrailleCoords canvas.Width canvas.Height    
        seq {
            for y in [0..brailleCanvas.Y-1] do
                for x in [0..brailleCanvas.X-1] do
                    match canvas.Grid |> Map.tryFind { X = x; Y = y } with
                    | Some braille -> yield brailleToString braille
                    | None -> yield " "

                yield "\n"
        }
