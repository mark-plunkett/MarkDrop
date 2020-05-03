module ConViz

    open Drawille
    open Drawille.Shapes

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

    let animateState animator initialCanvas initialUserState =

        let fps = 30.
        let msPerFrame = 1000. / fps
        let timer = new System.Diagnostics.Stopwatch()
        let initialFrameState = {
            FrameCount = 1
            FrameStartMs = 0L
        }
        
        let rec drawFrame animator frameState currentCanvas userState = 

            let elapsedMs = timer.ElapsedMilliseconds - frameState.FrameStartMs

            printDebugInfo elapsedMs

            let sleepMs = msPerFrame - float elapsedMs
            if sleepMs > 0. then
                Threading.Thread.Sleep(int sleepMs)

            let startMs = timer.ElapsedMilliseconds
            let (nextCanvas, nextUserState) = animator frameState currentCanvas userState

            updateConsole nextCanvas

            let nextFrameState = { 
                frameState with 
                    FrameCount = frameState.FrameCount + 1; 
                    FrameStartMs = startMs
            }

            drawFrame animator nextFrameState nextCanvas nextUserState |> ignore

        timer.Start()
        drawFrame animator initialFrameState initialCanvas initialUserState

    //let animate animator initialCanvas =

    //    let initialState = {
    //        FrameCount = 1
    //        FrameStartMs = 0L
    //        UserState = None
    //    }

    //    let nullStateAnimator animator = 
    //        animator

    //    animateState animator initialCanvas initialState

    let slowFill state canvas = 
        // fills with vertical lines, 1 per frame
        let x = state.FrameCount % int canvas.Width
        drawLine (pixel x 0) (pixel x ((int canvas.Height) - 1)) canvas
    
    let rotateRect state canvas =
        // draws and rotates a square in centre of canvas
        let aRadians = ((2. * System.Math.PI) / 180.) * float state.FrameCount
        let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
        let rectDimensions = {| Width = 50; Height = 50 |}
        let rectOffsets = {| XOffset = rectDimensions.Width/2; YOffset = rectDimensions.Height/2 |}
    
        let rect = {
            A = pixel (-rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
            B = pixel (+rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
            C = pixel (+rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
            D = pixel (-rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
        }
    
        canvas
        |> clear
        |> drawRect rect
    
    let rotateLine state canvas =
        
        let aRadians = ((2. * System.Math.PI) / 360.) * float state.FrameCount
        let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
        let length = 50.    
        let pos = pixel (int length) 0 |> rotate aRadians |> translate origin
        canvas |> clear |> Drawille.drawLine origin pos