module ConViz

    open Drawille

    open System

    let updateConsole canvas =
        Console.SetCursorPosition(0, 0)
        let value = 
            canvas
            |> Drawille.toStrings
            |> Seq.reduce (+)
        Console.Write(value)
       
    let updateConsolePos charX charY (value: string) =
        Console.SetCursorPosition(charX, charY)
        Console.Write(value)

    let updateConsoleDiff prevCanvas nextCanvasFactory = 
        let prevGrid = Array2D.copy prevCanvas.Grid
        let nextCanvas = nextCanvasFactory prevCanvas
        let xOffset = int (prevCanvas.OriginX / pixelsPerBrailleX)
        let yOffset = int (prevCanvas.OriginY / pixelsPerBrailleY)
        Drawille.enumerate2 prevGrid nextCanvas.Grid (fun (charX, charY) oldValue newValue -> 
            if oldValue <> newValue then updateConsolePos (charX + xOffset) (charY + yOffset) (Drawille.brailleToString newValue)
        )

        nextCanvas