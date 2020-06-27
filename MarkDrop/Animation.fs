module Animation

    open ConViz
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

    module Phase =

        type AnimationState = {
            SampleBytes: byte[]
            PreviousSamples: int[,][]
        }

        let phase fileName =

            let convas = ConViz.initialise
            let canvas = Drawille.createCharCanvas convas.CharWidth (convas.CharHeight / 2)

            let wavHeader = WavAudio.readHeader fileName false
            let sampleInfo = WavAudio.getSampleInfo wavHeader

            let p = 9
            let blockSize = pown 2 9
            let blockSizeBytes = blockSize * wavHeader.BlockAlign
            let sampleMax = 0.5 *  pown 2. 16
            let ampScalingFactor = (float canvas.Height / sampleMax)
            let halfPi = Math.PI / 2.
            let origin = Drawille.pixel (int canvas.Width / 2) 0
            let smoothing = 5

            let stateAggregator oldData newData =
                { oldData with SampleBytes = Array.append oldData.SampleBytes newData }

            let translateX value =
                value + float origin.X

            let translateY value = 
                float canvas.Height - 1. - value

            let smooth previousSamples samples = 
                
                let numChannels = Array2D.length1 samples
                let numSamplesPerChannel = Array2D.length2 samples
                let numArrays = smoothing + 1

                let folder finalArray (offset, nextArray) =
                    Array2D.blit nextArray 0 0 finalArray 0 offset numChannels numSamplesPerChannel
                    finalArray

                Array.append previousSamples [|samples|]
                |> Array.mapi (fun i a -> (i * numSamplesPerChannel), a)
                |> Array.fold folder (Array2D.zeroCreate numChannels (numSamplesPerChannel * numArrays))

            let draw (samples : int[,]) =

                [0..Array2D.length2 samples - 1]
                |> List.map (fun i -> 
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

            let phaseAnimator canvas frameState = 

                let rec processBlock animationState =

                    let (bytes, nextBytes) = Util.chunkBytes blockSizeBytes animationState.SampleBytes
                    let samples = WavAudio.bytesToSamples sampleInfo bytes

                    samples
                    |> smooth animationState.PreviousSamples
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

            Vizualizer((phaseAnimator canvas), stateAggregator, initialUserState).Start

    module Spectrum =

        type AnimationState = {
            SampleBytes: byte[]
            PreviousSamples: float[][]
        }

        let spectrum fileName =

            let convas = ConViz.initialise
            let canvas = Drawille.createCharCanvas convas.CharWidth (convas.CharHeight / 2)

            let wavHeader = WavAudio.readHeader fileName false
            let sampleInfo = WavAudio.getSampleInfo wavHeader

            let p = 11
            let fftBlockSize = pown 2 p
            let fftOutputRatio = 0.5 // We discard half the FFT output
            let fftPaddedSize = pown 2 13
            let fftOutputSize = fftOutputRatio * float fftPaddedSize //float fftBlockSize * fftOutputRatio
            let fftBlockSizeBytes = fftBlockSize * wavHeader.BlockAlign
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

            Vizualizer((fftAnimator canvas), stateAggregator, initialUserState).Start