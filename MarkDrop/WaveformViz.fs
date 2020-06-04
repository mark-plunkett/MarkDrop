module WaveformViz

    open Drawille
    open WavAudio
    open ConViz

    open FSharpx.Collections
    open FSharp.Collections.ParallelSeq

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
        |> List.averageBy (fun i -> samples2D.[i,*] |> Seq.map float |> PSeq.max)

    let pointFolder convas c (p1, p2) =
        ConViz.updateConsoleDiff convas c (fun nextCanvas -> Drawille.drawLine p1 p2 nextCanvas)

    let padTo max element list =
        List.pad (max - List.length list) element list

    let translate offset value =
        offset + value

    let inline scale scalingFactor value =
        (float value * scalingFactor) |> int

    let drawWaveformScaled canvas range values =

        let translateY value = int canvas.Height - 1 - value
        let scaleY = float canvas.Height / range |> scale

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
        |> List.mapi (fun i v -> pixel i (int v |> scaleY |> translateY))
        |> Util.flip Drawille.drawTurtle canvas

    let drawMonoSumWaveform canvas range samples =

        let translateY = int canvas.Height / 2 |> translate 
        let scaleY = float canvas.Height / (range * 2.) |> scale

        samples
        |> Seq.map (snd >> avgChannels)
        |> Seq.mapi (fun i v -> 
            let y = abs v |> int |> scaleY
            (
                pixel i (translateY y),
                pixel i (translateY -y)
            )
        )
        |> Seq.fold (fun c (min, max) -> Drawille.drawLine min max c) canvas

    let averageChannels samples =
        [0..(Array2D.length2 samples) - 1]
        |> List.map (fun i -> Array.average (samples.[*,i] |> Array.map (float)))

