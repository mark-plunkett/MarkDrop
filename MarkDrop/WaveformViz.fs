module WaveformViz

    open Drawille
    open WavAudio

    open FSharp.Collections.ParallelSeq

    type Convas = {
        ScalingFactorY: int
        ZeroOffsetY: int
    }

    let getChunkSize wavHeader canvas =
        let sampleInfo = getSampleInfo wavHeader
        if sampleInfo.NumSamples <= int canvas.Width then sampleInfo.NumSamples
        else ceil (float sampleInfo.NumSamples / float canvas.Width) |> int

    let rec minMaxValues min max samples =
        match samples with
        | [] -> (min, max)
        | s::ss -> 
            if s > max then minMaxValues min s ss
            else if s < min then minMaxValues s max ss
            else minMaxValues min max ss

    let minMaxValues2D (samples2D:int[,]) =
        let flattenedSamples =
            [0..(Array2D.length1 samples2D) - 1]
            |> PSeq.map (fun i -> samples2D.[i,*])
            |> Array.concat

        (PSeq.min flattenedSamples, PSeq.max flattenedSamples)

    let normalizeY convas i = 
        int (float i / float -convas.ScalingFactorY) + convas.ZeroOffsetY

    let pointFolder c (p1, p2) =
        ConViz.updateConsoleDiff c (fun nextCanvas -> Drawille.drawLine p1 p2 nextCanvas)

    let minMaxToPixels convas x min max =
        let pMin = pixel x (normalizeY convas (float min))
        let pMax = pixel x (normalizeY convas (float max))
        (pMin, pMax)

    let minMaxPixelMapperi convas (i, samples) =
        let (min, max) = minMaxValues2D samples
        minMaxToPixels convas i min max

    let parallelMinMax convas canvas seq = 
        let mapper = minMaxPixelMapperi convas
        seq
        |> Seq.map mapper
        |> Seq.fold pointFolder canvas

    let minMax convas canvas seq = 
        seq
        |> Seq.mapi (fun i s -> 
            //printfn "%i" i
            //printfn "min %i, max %i" min max
            let (min, max) = minMaxValues2D s
            let pMin = pixel i (normalizeY convas (float min))
            let pMax = pixel i (normalizeY convas (float max))
            let zero = pixel i (normalizeY convas 0.0)
            (zero, pMin, pMax)
        )
        |> Seq.fold (fun c (z, p1, p2) -> 
            ConViz.updateConsole c
            Drawille.drawLine z p1 c
            |> Drawille.drawLine z p2
            ) canvas

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

    
