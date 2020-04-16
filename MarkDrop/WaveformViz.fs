module WaveformViz

    open Drawille
    open WavAudio

    open FSharp.Collections.ParallelSeq

    type Convas = {
        ScalingFactorY: float
        ZeroOffsetY: int
    }

    let convasFromCanvas canvas maxY = {
        ScalingFactorY = maxY / float canvas.Height
        ZeroOffsetY = int canvas.Height / 2
    }

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
        int (float i / float -scalingFactor) + offset

    let normalizeY convas i = 
        normalize convas.ScalingFactorY convas.ZeroOffsetY i

    let pointFolder c (p1, p2) =
        ConViz.updateConsoleDiff c (fun nextCanvas -> Drawille.drawLine p1 p2 nextCanvas)

    let mapToPixel convas x y =
        pixel x (normalizeY convas y)

    let minMaxToPixels convas x min max =
        let pMin = mapToPixel convas x min
        let pMax = mapToPixel convas x max
        (pMin, pMax)

    let minMaxPixelMapperi convas (i, samples) =
        let (min, max) = minMaxValues2D samples
        minMaxToPixels convas i min max

    let drawWaveform canvas values =
        let (minY, maxY) = minMaxValues values
        let scalingFactoryY = (max (abs maxY) (abs minY)) / (float canvas.Height / 2.)
        let zeroOffsetY = int canvas.Height / 2
        let length = List.length values
        if length <= int canvas.Width then 
            values
            |> List.mapi (fun i v -> pixel i (normalize scalingFactoryY zeroOffsetY v))
            |> Util.flip Drawille.turtle canvas
        else
            let count = ceil (float length / float canvas.Width) |> int
            values
            |> List.chunkBySize count
            |> List.map Seq.average
            //|> Util.iterTrans (fun i -> printfn "%A" i)
            |> List.mapi (fun i v -> pixel i (normalize scalingFactoryY zeroOffsetY v))
            |> Util.flip Drawille.turtle canvas

    let drawMonoSum convas canvas samples =
        samples
        |> Seq.map (fun samples -> snd samples|> avgChannels)
        |> Seq.mapi (fun i v -> minMaxToPixels convas i v -v)
        |> Seq.fold (fun c (min, max) -> Drawille.drawLine min max c) canvas

    let drawWaveformSamples convas canvas samples = 
        let mapper = minMaxPixelMapperi convas
        samples
        |> Seq.map mapper
        |> Seq.fold pointFolder canvas

    let averageChannels samples =
        [0..(Array2D.length2 samples) - 1]
        |> List.map (fun i -> Array.average (samples.[*,i] |> Array.map (float)))

    let traceWaveformSamples convas canvas (samples: int[,]) =
        
        samples
            |> averageChannels
            |> List.splitInto (min (int canvas.Width) (samples |> Array2D.length2))
            |> List.mapi (fun i s -> pixel i (List.average s |> normalizeY convas))
            //|> Util.iterTrans (fun p -> printfn "%A" p)
            |> List.pairwise
            |> List.fold pointFolder canvas

    let naieveAverage convas canvas seq = 
        seq
        |> Seq.mapi (fun i s -> 
            //printfn "%i" i
            s
            |> averageChannels
            |> List.map (fun amplitude -> Drawille.pixel i (amplitude |> normalizeY convas))
            |> Util.unique
        )
        |> Seq.collect id
        |> Seq.pairwise
        |> Seq.fold pointFolder canvas

    
