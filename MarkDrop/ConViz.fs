module ConViz

    open Drawille
    open Drawille.Shapes

    open System

    type Convas = {
        Width: int
        Height: int
        CanvasWidth: int
        CanvasHeight: int
        OriginalCursorY: int
        CursorEndY: int
    }

    let initialise =
        Console.OutputEncoding <- Text.Encoding.UTF8
        Console.CursorVisible <- false
        let w = Console.WindowWidth - 1
        let h = Console.WindowHeight - 1
        {
            Width = w
            Height = h
            CanvasWidth = w * 2
            CanvasHeight = h * 4
            OriginalCursorY = Console.CursorTop
            CursorEndY = Console.CursorTop + h
        }

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

    type FrameState<'UserState> = {        
        FrameCount: int
        FrameStartMs: int64
        FrameDurationMs: int64
        TotalMs: int64
        TickCount: int64
        UserState: 'UserState
    }

    let printDebugInfo frameState =
        let fps = if frameState.FrameDurationMs = 0L then 0L else 1000L / frameState.FrameDurationMs
        updateConsolePos 0 0 (sprintf "frame#: %i    ms/frame: %i    FPS: %i" frameState.FrameCount frameState.FrameDurationMs fps)

    let animateState animator initialCanvas initialUserState : unit =

        let tickRate = 1000.
        let timer = new System.Diagnostics.Stopwatch()
        let initialFrameState = {
            FrameCount = 1
            FrameStartMs = 0L
            FrameDurationMs = 0L
            TotalMs = 0L
            TickCount = 0L
            UserState = []
        }
        let startTime = DateTime.UtcNow
        
        let rec drawFrame animator frameState currentCanvas userState = 

            let frameState' = { frameState with FrameDurationMs = timer.ElapsedMilliseconds - frameState.FrameStartMs }
            let startMs = timer.ElapsedMilliseconds
            let next = animator frameState' currentCanvas userState
            match next with
            | None -> ()
            | Some (nextCanvas, nextUserState) -> 

                printDebugInfo frameState

                let nextFrameState = { 
                    frameState' with 
                        FrameCount = frameState.FrameCount + 1; 
                        FrameStartMs = startMs
                        TotalMs = (DateTime.UtcNow - startTime).TotalMilliseconds |> int64
                        TickCount = (DateTime.UtcNow - startTime).TotalSeconds * tickRate |> int64
                }

                drawFrame animator nextFrameState nextCanvas nextUserState

        timer.Start()
        drawFrame animator initialFrameState initialCanvas initialUserState

    let slowFill state (canvas: Drawille.Canvas) = 
        // fills with vertical lines, 1 per frame
        let x = state.FrameCount % int canvas.Width
        drawLine (pixel x 0) (pixel x ((int canvas.Height) - 1)) canvas
    
    let rotateRect (canvas: Drawille.Canvas) frameState =
        // draws and rotates a square in centre of canvas
        let aRadians = ((2. * System.Math.PI) / 180.) * float frameState.FrameCount
        let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
        let l = 50. + (sin <| float frameState.FrameCount) * 100. |> int
        let rectDimensions = {| Width = l; Height = l |}
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
    
    let rotateLine state (canvas: Drawille.Canvas) =
        
        let aRadians = ((2. * System.Math.PI) / 360.) * float state.FrameCount
        let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
        let length = 50.    
        let pos = pixel (int length) 0 |> rotate aRadians |> translate origin
        canvas |> clear |> Drawille.drawLine origin pos