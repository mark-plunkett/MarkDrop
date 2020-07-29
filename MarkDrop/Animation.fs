module Animation

    open Drawille
    open ConViz
    open WavAudio
    
    open System

    let storeSamples smoothing previousSamples samples =
        if smoothing = 0 then
            [||]
        else
            match Array.length previousSamples with
            | length when length = smoothing -> Array.append (Array.tail previousSamples) [|samples|]
            | _ -> Array.append previousSamples [|samples|]

    let throttleFps frameState = 
        let targetFps = 60.
        let elapsedSeconds = (float frameState.ElapsedMs / 1000.)
        let diff = float frameState.FrameCount - (targetFps * elapsedSeconds) |> int
        if diff > 0 then
            Threading.Thread.Sleep(diff)
    
    let animate<'TUserState> sampleInfo (stream: IO.Stream) (viz: MailboxProcessor<VizMessage<'TUserState, byte[]>>) forever =

        let latencyMs = 30.
        let numSamples = sampleInfo.SampleRate / int latencyMs
        let numBytesRaw = sampleInfo.BytesPerSample * numSamples
        let numBytes = numBytesRaw - (numBytesRaw % sampleInfo.BytesPerMultiChannelSample)
        let expectedBytesPerMs = (float sampleInfo.SampleRate * float sampleInfo.BytesPerMultiChannelSample) / 1000.

        let calculateLatency totalBytesProcessed frameState =
            let expectedBytesProcessed = float frameState.ElapsedMs * expectedBytesPerMs
            (float totalBytesProcessed - expectedBytesProcessed) / expectedBytesPerMs
        
        let buffer = Array.zeroCreate numBytes
        let rec animateBytes totalBytesProcessed = async {

            let msToWait = 
                viz.PostAndReply (fun replyChannel ->  Reply(replyChannel)) 
                |> calculateLatency totalBytesProcessed 
                |> max 0. 
                |> int
            do! Async.Sleep msToWait

            match stream.Read(buffer, 0, numBytes) with
            | 0 -> 
                if forever then return! animateBytes totalBytesProcessed
                else ()
            | bytesRead -> 
                viz.Post (Data buffer)
                return! animateBytes (totalBytesProcessed + bytesRead)

        }

        animateBytes 0 
        |> Async.RunSynchronously
        |> ignore

    module Feedback =

        type AnimationState = {
            SampleBytes: byte[]
            RotateOrigin: Drawille.Pixel
        }

        module internal Internal =

            let convas = { ConViz.initialise() with ZeroOrigin = pixel 0 0 }
            let canvas = Drawille.createCharCanvas convas.CharWidth convas.CharHeight
            let fastConsole = FastConsole.FastConsole(convas.CharWidth, convas.CharHeight)
            let origin = Drawille.pixel (int canvas.Width / 2) (int canvas.Height / 2)
            let p = 9
            let blockSize = pown 2 p

            let r = Random()

            let pixels = Array2D.zeroCreate<bool> (int canvas.Width) (int canvas.Height)

            let centre origin pixels =
                pixels
                |> Array.map (Drawille.translate origin)

            let drawCenterdSquare origin x =
                Drawille.rect {
                    A = Drawille.pixel (x/2) (x/2) 
                    B = Drawille.pixel (-x/2) (x/2)
                    C = Drawille.pixel (-x/2) (-x/2)
                    D = Drawille.pixel (x/2) (-x/2)
                }
                |> centre origin

            let iterPixels pixels f =
                for x = 0 to Array2D.length1 pixels - 1 do
                    for y = 0 to Array2D.length2 pixels - 1 do
                        f x y

            let filterPixels f pixels =
                seq {
                    for x = 0 to Array2D.length1 pixels - 1 do
                        for y = 0 to Array2D.length2 pixels - 1 do
                            if f x y then yield x, y
                }

            let jitterPixel x y =

                let j = 1
                pixel (r.Next(-j, j) + x) (r.Next(-j, j) + y)

            let pixelsToCanvas pixels canvas t =
                iterPixels pixels (fun x y ->
                    if pixels.[x,y] then  
                        let f = 
                            match t % 1000 with
                            | x when x > 800 -> Drawille.set
                            | _ -> Drawille.toggle
                        f (jitterPixel x y) canvas |> ignore
                        //Drawille.set (jitterPixel x y) canvas |> ignore
                    else
                        Drawille.unset (pixel x y) canvas |> ignore
                )
                canvas

            let setPixel x y v =
                let xInRange = x >= 0 && x < Array2D.length1 pixels
                let yInRange = y >= 0 && y < Array2D.length2 pixels
                if xInRange && yInRange then
                    pixels.[x,y] <- v

            let dumpPixels ps =
                
                Console.SetCursorPosition(0,0)
                for y = 0 to Array2D.length2 ps - 1 do
                    for x = 0 to Array2D.length1 ps - 1 do
                        let c = 
                            if x = int origin.X && y = int origin.Y then
                                "c"
                            else
                                match ps.[x,y] with
                                | true -> "x"
                                | false -> "-"

                        printf "%s" c

                    printfn ""
            
            let rotate angle x y =
                let x = float x * sin angle |> int
                let y = float y * cos angle |> int
                x, y

            let fps = 60.
            let msPerFrame = 1000. / fps

            let rotateOrigin = Drawille.pixel (int origin.X + 20) (int origin.Y + 20)
            let angle = 1. * Math.PI / 180.

            let translateToOrigin origin x y =
                (x - int origin.X), (y - int origin.Y)

            let zoom x y =

                x, y * 1.01

            let rotateCoords origin angle x y  =
                let tX, tY = translateToOrigin origin x y
                //printfn "tx: %i ty: %i" tX tY
                let newX, newY = rotateCoords angle tX tY ||> zoom
                //printfn "newX: %f newY: %f" newX newY
                let xx, yy = translateCoords origin (newX |> round) (newY |> round)
                //printfn "xx: %i yy: %i" xx yy   

                xx,yy

            let animatePixels pixels rotateOrigin =

                pixels 
                |> filterPixels (fun x y -> pixels.[x,y]) 
                |> Array.ofSeq
                // |> Util.tee (fun ps -> 
                //     Console.SetCursorPosition(0, 50)
                //     ps |> Seq.length |> printfn "ps count: %i")
                |> Seq.iter (fun (x, y) -> 
                    pixels.[x,y] <- false
                    let finalX, finalY = rotateCoords rotateOrigin angle x y 
                    setPixel finalX finalY true
                )

            let fadePixels pixels =

                pixels 
                |> filterPixels (fun x y -> pixels.[x,y]) 
                |> Seq.iter (fun (x, y) -> 
                    pixels.[x,y] <- false
                    if r.NextDouble() > 0.2 then
                        setPixel x y true
                )

            let drawLineSpectrum samples =

                let topFrom = pixel 0 (int origin.Y - 1)
                let topTo = pixel (int canvas.Width - 1) (int origin.Y - 1)
                let bottomFrom = pixel 0 (int origin.Y + 1)
                let bottomTo = pixel (int canvas.Width - 1) (int origin.Y + 1)
                let midFrom = pixel 0 (int origin.Y)
                let midTo = pixel (int canvas.Width - 1) (int origin.Y)

                let t = line topFrom topTo
                let b = line bottomFrom bottomTo

                Array.concat [| 
                    t;b
                |]
                |> Array.iter (fun p -> setPixel (int p.X) (int p.Y) true)

                line midFrom midTo
                |> Array.iter (fun p -> setPixel (int p.X) (int p.Y) false)

            let stateAggregator oldData newData =
                { oldData with SampleBytes = Array.append oldData.SampleBytes newData }

            let feedback sampleInfo =

                let blockSizeBytes = blockSize * sampleInfo.BytesPerMultiChannelSample

                let feedbackAnimator frameState = 

                    animatePixels pixels origin
                    //fadePixels pixels

                    let rec processBlock animationState = 

                        let (bytes, nextBytes) = Util.chunkBytes blockSizeBytes animationState.SampleBytes
                        let samples = WavAudio.bytesToSamples sampleInfo bytes

                        animatePixels pixels animationState.RotateOrigin
                        
                        samples |> drawLineSpectrum 
                        pixelsToCanvas pixels canvas (frameState.ElapsedMs |> int) |> ignore
                        let chars = canvas.Grid |> Array2D.map (brailleToChar >> int16)
                        fastConsole.WriteChars(chars)

                        let userState' = { 
                            animationState with 
                                SampleBytes = nextBytes
                                RotateOrigin = rotateCoords origin (5. * Math.PI / 180.) (int animationState.RotateOrigin.X) (int animationState.RotateOrigin.Y) |> Drawille.pixelFromCoords
                        }

                        match nextBytes with
                        | [||] -> userState'
                        | _ -> processBlock userState'

                    processBlock frameState.UserState

                let initialUserState = {
                    SampleBytes = Array.zeroCreate 0
                    RotateOrigin = rotateOrigin
                }

                Vizualizer(feedbackAnimator, stateAggregator, initialUserState)

        let feedback sampleInfo =

            Internal.feedback sampleInfo

    module Phase =
        
        type AnimationState = {
            SampleBytes: byte[]
            PreviousSamples: int[,][]
        }

        module internal Internal =

            let convas = ConViz.initialise()
            let canvas = Drawille.createCharCanvas convas.CharWidth convas.CharHeight
            let p = 9
            let blockSize = pown 2 p
            let sampleMax = 0.5 *  pown 2. 16            
            let smoothing = 5
            let ampScalingFactor = float canvas.Height / sampleMax
            let halfPi = Math.PI / 2.
            let origin = Drawille.pixel (int canvas.Width / 2) 0

            let translateX value =
                value + float origin.X

            let translateY value = 
                float canvas.Height - 1. - value

            let smooth smoothing previousSamples samples = 
                
                let numChannels = Array2D.length1 samples
                let numSamplesPerChannel = Array2D.length2 samples
                let numArrays = smoothing + 1

                let folder finalArray (offset, nextArray) =
                    Array2D.blit nextArray 0 0 finalArray 0 offset numChannels numSamplesPerChannel
                    finalArray

                Array.append previousSamples [|samples|]
                |> Array.mapi (fun i a -> (i * numSamplesPerChannel), a)
                |> Array.fold folder (Array2D.zeroCreate numChannels (numSamplesPerChannel * numArrays))

            let stateAggregator oldData newData =
                { oldData with SampleBytes = Array.append oldData.SampleBytes newData }

            let draw (samples : int[,]) =

                [|0..Array2D.length2 samples - 1|]
                |> Array.map (fun i -> 
                    let slice = samples.[*,i]
                    let l = float slice.[0]
                    let r = float slice.[1]
                    let lAbs = l |> abs |> float
                    let rAbs = r |> abs |> float
                    let furthestFromZero = 
                        if lAbs > rAbs then l
                        else if rAbs > lAbs then r
                        else if rAbs = 0. then 1.
                        else r
                    let amplitude = ((lAbs + rAbs) / 2.) * ampScalingFactor
                    let angle = ((-l + r) / furthestFromZero) + halfPi
                    let x = amplitude * cos angle 
                    let xPos = -x |> translateX |> int
                    let y = amplitude * sin angle 
                    let yPos = y |> translateY |> int
                    Drawille.pixel xPos yPos)
                |> Util.flip Drawille.drawPoints (canvas |> Drawille.clear)
                |> ConViz.updateConsole convas

            let phase sampleInfo =

                let blockSizeBytes = blockSize * sampleInfo.BytesPerMultiChannelSample

                let phaseAnimator frameState = 

                    let rec processBlock animationState =

                        let (bytes, nextBytes) = Util.chunkBytes blockSizeBytes animationState.SampleBytes
                        let samples = WavAudio.bytesToSamples sampleInfo bytes

                        samples
                        |> smooth smoothing animationState.PreviousSamples
                        |> draw

                        let userState' = { 
                            animationState with 
                                SampleBytes = nextBytes
                                PreviousSamples = storeSamples smoothing animationState.PreviousSamples samples
                        }
                        
                        match nextBytes with
                        | [||] -> userState'
                        | _ -> processBlock userState'

                    processBlock frameState.UserState

                let initialUserState = {
                    SampleBytes = Array.zeroCreate 0
                    PreviousSamples = Array.empty
                }

                Vizualizer(phaseAnimator, stateAggregator, initialUserState)

        let phaseRaw sampleInfo =

            Internal.phase sampleInfo

    module Spectrum =

        type AnimationState = {
            SampleBytes: byte[]
            PreviousSamples: float[][]
        }

        let spectrum sampleInfo =

            let convas = ConViz.initialise()
            let canvas = Drawille.createCharCanvas convas.CharWidth (convas.CharHeight / 2)

            let p = 11
            let fftBlockSize = pown 2 p
            let fftOutputRatio = 0.5 // We discard half the FFT output
            let fftPaddedSize = pown 2 13
            let fftOutputSize = fftOutputRatio * float fftPaddedSize
            let fftBlockSizeBytes = fftBlockSize * sampleInfo.BytesPerMultiChannelSample
            let yScalingFactor = 2.
            let sampleScalingFactor = 0.5 * pown 2. 16
            let slope = 4.
            let slopeScale = slope / float canvas.Width
            let smoothing = 3
            let skip = 2

            let scaleX value = 
                value
                |> Util.logScale fftOutputSize
                |> WaveformViz.scale (float canvas.Width / fftOutputSize) 
                |> float

            let translateY value = 
                int canvas.Height - 1 - value

            let applySlope xPos value =
                value * xPos * slopeScale

            let scaleY xPos value = 
                value * yScalingFactor
                |> applySlope xPos

            let stateAggregator oldData newData =
                { oldData with SampleBytes = Array.append oldData.SampleBytes newData }

            let aggregateSamples (samples: int[,]) =
                // TODO: this is only using left channel atm
                samples.[0,*]

            let fftAnimator canvas frameState = 

                let drawSpectrum samples = 
                    samples
                    |> FFFT.fftFloatToFloat
                    |> Array.take (int fftOutputSize)
                    |> Array.skip 20
                    |> Array.mapi (fun i v -> 
                        let i' = i - skip
                        let xPos = i' |> float |> scaleX |> int
                        let yPos = v |> abs |> scaleY <| float xPos |> int |> translateY
                        Drawille.pixel xPos yPos)
                    |> Util.flip Drawille.drawTurtle (canvas |> Drawille.clear)
                    |> ConViz.updateConsole convas

                let smooth previousSamples samples = 
                    previousSamples
                    |> Array.append [|samples|]
                    |> Array.transpose
                    |> Array.map Array.average

                let rec processBlock animationState =

                    throttleFps frameState |> ignore

                    let (bytes, nextBytes) = Util.chunkBytes fftBlockSizeBytes animationState.SampleBytes
                    let samples = 
                        WavAudio.bytesToSamples sampleInfo bytes
                        |> aggregateSamples
                        |> Util.pad fftPaddedSize
                        |> Util.normalize sampleScalingFactor

                    samples
                    |> smooth animationState.PreviousSamples
                    |> drawSpectrum 
                    |> ignore

                    let userState' = { 
                        animationState with 
                            SampleBytes = nextBytes
                            PreviousSamples = storeSamples smoothing animationState.PreviousSamples samples
                    }

                    match nextBytes with
                    | [||] -> userState'
                    | _ -> processBlock userState'

                processBlock frameState.UserState

            let initialUserState = {
                SampleBytes = Array.zeroCreate 0
                PreviousSamples = Array.empty
            }

            Vizualizer((fftAnimator canvas), stateAggregator, initialUserState)