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

    type FrameState<'UserState> = {        
        FrameCount: int
        FrameStartMs: int64
        FrameDurationMs: int64
        TickCount: int64
        UserState: 'UserState
    }

    let updateConsole canvas =
        Console.SetCursorPosition(int canvas.OriginX, int canvas.OriginY)
        canvas
            |> Drawille.toStrings
            |> Console.Write
       
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

    let printDebugInfo frameState =
        let fps = if frameState.FrameDurationMs = 0L then 0L else 1000L / frameState.FrameDurationMs
        updateConsolePos 0 0 (sprintf "frame#: %i    ms/frame: %i    FPS: %i" frameState.FrameCount frameState.FrameDurationMs fps)

    type Vizualizer (animator, userStateAggregator, initialUserState) =

        let dataAgent msPerFrame animator initialUserState = 

            let initialFrameState = {
                FrameCount = 1
                FrameStartMs = 0L
                FrameDurationMs = 0L
                TickCount = 0L
                UserState = initialUserState
            }

            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()

            MailboxProcessor.Start(fun inbox ->    
        
                let rec loop currentFrameState = async {
                    let! msg = inbox.TryReceive(msPerFrame)
                
                    let currentFrameState' = 
                        { currentFrameState with 
                            UserState = userStateAggregator currentFrameState.UserState msg }
                
                    animator currentFrameState' 
            
                    let elapsedMs = timer.ElapsedMilliseconds
                    let nextFrameState = { 
                        currentFrameState' with 
                            FrameCount = currentFrameState'.FrameCount + 1
                            FrameStartMs = elapsedMs
                            FrameDurationMs = elapsedMs - currentFrameState.FrameStartMs
                            TickCount = elapsedMs
                    }

                    printDebugInfo nextFrameState

                    return! loop nextFrameState
                }

                loop initialFrameState
            )

        member _.Start = 
            let msPerFrame = 1
            dataAgent msPerFrame animator initialUserState

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

    let drawCanvas canvas =
        canvas
        |> Drawille.toStrings
        |> printfn "%s"

    let slowFill state (canvas: Drawille.Canvas) = 
        // fills with vertical lines, 1 per frame
        let x = state.FrameCount % int canvas.Width
        drawLine (pixel x 0) (pixel x ((int canvas.Height) - 1)) canvas
    
    let rotateRect (canvas: Drawille.Canvas) frameState =
        // draws and rotates a square in centre of canvas
        let aRadians = ((2. * System.Math.PI) / 180.) * float frameState.FrameCount
        let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
        let l = 50. + (sin <| (float frameState.FrameCount / 10.)) * 100. |> int
        let rectDimensions = {| Width = l; Height = l |}
        let rectOffsets = {| XOffset = rectDimensions.Width/2; YOffset = rectDimensions.Height/2 |}
    
        let rect = {
            A = pixel (-rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
            B = pixel (+rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
            C = pixel (+rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
            D = pixel (-rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
        }
    
        rect
        |> Drawille.rect
        |> Util.flip Drawille.togglePoints canvas
    
    let rotateLine state (canvas: Drawille.Canvas) =
        
        let aRadians = ((2. * System.Math.PI) / 360.) * float state.FrameCount
        let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
        let length = 50.    
        let pos = pixel (int length) 0 |> rotate aRadians |> translate origin
        canvas |> clear |> Drawille.drawLine origin pos