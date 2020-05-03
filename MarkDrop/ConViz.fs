module ConViz

    open Drawille

    open System

    let updateConsole canvas =
        Console.SetCursorPosition(int canvas.OriginX, int canvas.OriginY)
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

    let drawCanvas canvas =
        canvas
        |> Drawille.toStrings
        |> Seq.reduce (+)
        |> printfn "%s"

    type FrameState = {
        
        FrameCount: int
        FrameStartMs: int64
    }

    let printDebugInfo info =
        let fps = if info = 0L then 0L else 1000L / info
        updateConsolePos 0 0 (sprintf "FPS: %i" fps)

    let animate canvasFactory initialCanvas =

        let fps = 30.
        let msPerFrame = 1000. / fps
        let timer = new System.Diagnostics.Stopwatch()
        let initialState = {
            FrameCount = 1
            FrameStartMs = 0L
        }
        
        let rec drawFrame canvasFactory frameState currentCanvas = 

            let elapsedMs = timer.ElapsedMilliseconds - frameState.FrameStartMs

            printDebugInfo elapsedMs

            let sleepMs = msPerFrame - float elapsedMs
            if sleepMs > 0. then
                Threading.Thread.Sleep(int sleepMs)

            let nextFrameState = { 
                frameState with 
                    FrameCount = frameState.FrameCount + 1; 
                    FrameStartMs = timer.ElapsedMilliseconds}
            let nextCanvas = canvasFactory frameState currentCanvas
            updateConsole nextCanvas
            drawFrame canvasFactory nextFrameState nextCanvas |> ignore

        timer.Start()
        drawFrame canvasFactory initialState initialCanvas
