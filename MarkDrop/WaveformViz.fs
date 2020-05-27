module WaveformViz

    open Drawille
    open WavAudio
    open ConViz

    open FSharpx.Collections
    open FSharp.Collections.ParallelSeq

    // type Convas = {
    //     ScalingFactorY: float
    //     ZeroOffsetY: int
    // }

    // let convasFromCanvas canvas maxY = {
    //     ScalingFactorY = maxY / float canvas.Height
    //     ZeroOffsetY = int canvas.Height / 2
    // }

    let getChunkSize wavHeader canvas =
        let sampleInfo = getSampleInfo wavHeader
        if sampleInfo.NumSamples <= int canvas.Width then sampleInfo.NumSamples
        else ceil (float sampleInfo.NumSamples / float canvas.Width) |> int

    let inline minMaxValues values =
        (PSeq.min values, PSeq.max values)

    let inline minMaxValues2D samples2D =
        let flattenedSamples =
            [0..(Array2D.length1 samples2D) - 1]
            |> PSeq.map (fun i -> samples2D.[i,*])
            |> Array.concat

        minMaxValues flattenedSamples

    let inline avgChannels samples2D =
        [0..(Array2D.length1 samples2D) - 1]
        |> List.map (fun i -> samples2D.[i,*] |> Seq.map float |> PSeq.max)
        |> List.average

    let normalize scalingFactor offset i =
        int (float i / float scalingFactor) + offset

    let normalizePixel convas (canvas: Canvas) p =
        let scaleX = convas.MaxX / float canvas.Height
        let scaleY = convas.MaxY / float canvas.Width
        let scaledX = int p.X / int scaleX
        let scaledY = int p.Y / int scaleY
        pixel scaledX (int canvas.Height - scaledY)

    let normalizeY convas (canvas: Canvas) i = 
        let scalingFactor = convas.MaxY / float canvas.Height
        normalize scalingFactor (int convas.Origin.Y) i

    let pointFolder convas c (p1, p2) =
        ConViz.updateConsoleDiff convas c (fun nextCanvas -> Drawille.drawLine p1 p2 nextCanvas)

    let mapToPixel convas (canvas: Canvas) x y =
        pixel x (normalizeY convas canvas y)

    let minMaxToPixels convas (canvas: Canvas) x min max =
        let pMin = mapToPixel convas canvas x min
        let pMax = mapToPixel convas canvas x max
        (pMin, pMax)

    let minMaxPixelMapperi convas (i, samples) =
        let (min, max) = minMaxValues2D samples
        minMaxToPixels convas i min max

    let padTo max element list =
        List.pad (max - List.length list) element list

    // let drawWaveformYOffset convas (canvas: Canvas) yOffset values =
    //     let length = List.length values
    //     let values' = 
    //         if length <= int canvas.Width then 
    //             values
    //         else 
    //             let chunkSize = ceil (float length / float canvas.Width) |> int
    //             values
    //             |> List.chunkBySize chunkSize
    //             |> List.map (padTo chunkSize 0. >> Seq.average)

    //     let (minY, maxY) = minMaxValues values'
    //     values'
    //     //|> Util.iterTrans (fun i -> printfn "%f" i)
    //     |> List.mapi (fun i v -> pixel i (normalize convas canvas v))
    //     |> Util.flip Drawille.drawTurtle canvas

    let drawWaveformScaled convas (canvas: Canvas) values =
        let length = List.length values
        let values' = 
            if length <= int canvas.Width then 
                values
            else 
                let chunkSize = ceil (float length / float canvas.Width) |> int
                values
                |> List.chunkBySize chunkSize
                |> List.map (padTo chunkSize 0. >> Seq.average)

        values'
        |> List.mapi (fun i v -> pixel i (int v) |> normalizePixel convas canvas)
        |> Util.flip Drawille.drawTurtle canvas

    let drawMonoSumWaveform canvas range samples =

        let yOffset = int canvas.Height / 2

        let normalizer value =
            let scalingFactor = float canvas.Height / (range * 2.)
            (float value * scalingFactor) |> int

        samples
        |> Seq.map (snd >> avgChannels)
        |> Seq.mapi (fun i v -> 
            let y = abs v |> int |> normalizer
            (
                pixel i (y + yOffset),
                pixel i (-y + yOffset)
            )
        )
        |> Seq.fold (fun c (min, max) -> Drawille.drawLine min max c) canvas

    // let drawWaveformSamples convas (canvas: Canvas) samples = 
    //     let mapper = minMaxPixelMapperi convas
    //     samples
    //     |> Seq.map mapper
    //     |> Seq.fold (pointFolder convas) canvas

    let averageChannels samples =
        [0..(Array2D.length2 samples) - 1]
        |> List.map (fun i -> Array.average (samples.[*,i] |> Array.map (float)))

    // let traceWaveformSamples convas (canvas: Canvas) (samples: int[,]) =
        
    //     samples
    //         |> averageChannels
    //         |> List.splitInto (min (int canvas.Width) (samples |> Array2D.length2))
    //         |> List.mapi (fun i s -> pixel i (List.average s |> normalizeY convas canvas))
    //         //|> Util.iterTrans (fun p -> printfn "%A" p)
    //         |> List.pairwise
    //         |> List.fold (pointFolder convas) canvas

    // let naieveAverage convas (canvas: Canvas) seq = 
    //     seq
    //     |> Seq.mapi (fun i s -> 
    //         //printfn "%i" i
    //         s
    //         |> averageChannels
    //         |> List.map (fun amplitude -> Drawille.pixel i (amplitude |> normalizeY convas canvas))
    //         |> Util.unique
    //     )
    //     |> Seq.collect id
    //     |> Seq.pairwise
    //     |> Seq.fold (pointFolder convas) canvas

    
