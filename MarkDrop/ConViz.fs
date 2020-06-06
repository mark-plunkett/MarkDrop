module ConViz

    open Drawille
    open Drawille.Shapes

    open System

    type Convas = {
        CharWidth: int
        CharHeight: int
        ZeroOrigin: Pixel
        Origin: Pixel
        MaxX: float
        MaxY: float
    }

    type FrameState<'UserState> = {        
        FrameCount: int
        FrameStartMs: int64
        FrameDurationMs: int64
        ElapsedMs: int64
        UserState: 'UserState
    }

    type VizMessage<'TUserState, 'TMessage> =
    | Data of 'TMessage
    | Reply of AsyncReplyChannel<FrameState<'TUserState>>

    let updateConsole convas canvas =
        Console.SetCursorPosition(int convas.ZeroOrigin.X, 0)
        canvas
            |> Drawille.toStrings
            |> Console.Out.Write
       
    let updateConsolePos charX charY (value: string) =
        Console.SetCursorPosition(charX, charY)
        Console.Write(value)

    let updateConsoleDiff convas prevCanvas nextCanvasFactory = 
        let prevGrid = Array2D.copy prevCanvas.Grid
        let nextCanvas = nextCanvasFactory prevCanvas
        let xOffset = int (convas.ZeroOrigin.X / pixelsPerBrailleX)
        let yOffset = int (convas.ZeroOrigin.Y / pixelsPerBrailleY)
        Drawille.enumerate2 prevGrid nextCanvas.Grid (fun (charX, charY) oldValue newValue -> 
            if oldValue <> newValue then updateConsolePos (charX + xOffset) (charY + yOffset) (Drawille.brailleToString newValue)
        )

        nextCanvas

    let printDebugInfo frameState =
        let fps = if frameState.FrameDurationMs = 0L then 0L else 1000L / frameState.FrameDurationMs
        updateConsolePos 0 0 (sprintf "frame#: %i    ms/frame: %i    FPS: %i" frameState.FrameCount frameState.FrameDurationMs fps)

    type Vizualizer<'TUserState, 'TMessage> (animator, userStateAggregator, initialUserState) =

        let dataAgent animator initialUserState = 

            let initialFrameState: FrameState<'TUserState> = {
                FrameCount = 1
                FrameStartMs = 0L
                FrameDurationMs = 0L
                ElapsedMs = 0L
                UserState = initialUserState
            }

            let timer = System.Diagnostics.Stopwatch()
            timer.Start()

            MailboxProcessor<VizMessage<'TUserState, 'TMessage>>.Start(fun inbox ->    
        
                let rec loop currentFrameState = async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Reply replyChannel -> 
                        replyChannel.Reply(currentFrameState)
                        return! loop currentFrameState
                    | Data data -> 
                        let currentFrameState' = 
                            { currentFrameState with 
                                UserState = userStateAggregator currentFrameState.UserState data }
                    
                        let nextUserState = animator currentFrameState' 
                
                        let elapsedMs = timer.ElapsedMilliseconds
                        let nextFrameState = { 
                            currentFrameState' with 
                                FrameCount = currentFrameState'.FrameCount + 1
                                FrameStartMs = elapsedMs
                                FrameDurationMs = elapsedMs - currentFrameState.FrameStartMs
                                ElapsedMs = elapsedMs
                                UserState = nextUserState
                        }

                        printDebugInfo nextFrameState
                        return! loop nextFrameState
                }

                loop initialFrameState
            )

        member _.Start = 
            dataAgent animator initialUserState

    let initialise =
        Console.OutputEncoding <- Text.Encoding.UTF8
        Console.CursorVisible <- false
        let w = Console.WindowWidth - 1
        let h = Console.WindowHeight - 1
        {
            CharWidth = w
            CharHeight = h
            ZeroOrigin = pixel 0 Console.CursorTop
            Origin = pixel 0 0
            MaxX = 0.
            MaxY = 0.
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
        let l = 50. + (sin <| (float frameState.FrameCount / 100.)) * 100. |> int
        let rectDimensions = {| Width = l; Height = l |}
        let rectOffsets = {| XOffset = rectDimensions.Width/2; YOffset = rectDimensions.Height/2 |}
    
        let rect = {
            A = pixel (-rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
            B = pixel (+rectOffsets.XOffset) (-rectOffsets.YOffset) |> rotate aRadians |> translate origin
            C = pixel (+rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
            D = pixel (-rectOffsets.XOffset) (+rectOffsets.YOffset) |> rotate aRadians |> translate origin
        }
    
        let t = frameState.FrameCount % 20
        let f = 
            if t = 1 then Drawille.drawPoints
            else if t = 2 then Drawille.togglePoints
            else Drawille.erasePoints

        rect
        |> Drawille.rect
        |> Util.flip f canvas
    
    let rotateLine state (canvas: Drawille.Canvas) =
        
        let aRadians = ((2. * System.Math.PI) / 360.) * float state.FrameCount
        let origin = pixel (int canvas.Width / 2) (int canvas.Height / 2)
        let length = 50.    
        let pos = pixel (int length) 0 |> rotate aRadians |> translate origin
        canvas |> clear |> Drawille.drawLine origin pos