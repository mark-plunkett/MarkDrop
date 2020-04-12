module ConViz

    open Drawille
    open WavAudio

    open FSharp.Collections.ParallelSeq
    open System

    let getChunkSize wavHeader canvas =
        let sampleInfo = getSampleInfo wavHeader
        if sampleInfo.NumSamples <= int canvas.Width then sampleInfo.NumSamples
        else sampleInfo.NumSamples / int canvas.Width

    let averageChannels samples =
        [0..(Array2D.length2 samples) - 1]
        |> List.map (fun i -> Array.average (samples.[*,i] |> Array.map (float)))

    let rec minMaxValues min max samples =
        match samples with
        | [] -> (min, max)
        | s::ss -> 
            if s > max then minMaxValues min s ss
            else if s < min then minMaxValues s max ss
            else minMaxValues min max ss

    let minMaxValues2D samples2D =
        let flattenedSamples =
            [0..(Array2D.length1 samples2D) - 1]
            |> List.fold (fun flatList i -> flatList@(samples2D.[i,*] |> Array.toList)) []

        (PSeq.min flattenedSamples, PSeq.max flattenedSamples)

    let normalize scalingFactor offset i = 
        int (float i / float -scalingFactor) + offset

    let unique list =
        list
        |> List.fold (fun acc e ->
            match acc with
            | x::xs when x = e -> acc
            | _ -> e::acc) []
        |> List.rev

    let updateConsole canvas =
        Console.SetCursorPosition(0, 0)
        let value = 
            canvas
            |> Drawille.toStrings
            |> Seq.reduce (+)
        Console.Write(value)
       
    let updateConsolePos x y (value: string) =
        Console.SetCursorPosition(x, y)
        Console.Write(value)

    let updateConsoleDiff prevCanvas nextCanvasFactory = 
        let prevGrid = Array2D.copy prevCanvas.Grid
        let nextCanvas = nextCanvasFactory prevCanvas
        Drawille.enumerate2 prevGrid nextCanvas.Grid (fun (x, y) v1 v2 -> 
            if v1 <> v2 then updateConsolePos x y (Drawille.brailleToString v2)
        )

        nextCanvas

    let naieveAverage canvas scalingFactor offset seq = 
        seq
        |> Seq.mapi (fun i s -> 
            //printfn "%i" i
            s
            |> averageChannels
            |> List.map (fun amplitude -> Drawille.pixel i (amplitude |> normalize scalingFactor offset))
            |> unique
        )
        |> Seq.collect id
        |> Seq.pairwise
        |> Seq.fold (fun c (p1, p2) -> Drawille.drawLine p1 p2 c) canvas

    let minMax canvas scalingFactor offset seq = 
        seq
        |> Seq.mapi (fun i s -> 
            //printfn "%i" i
            //printfn "min %i, max %i" min max
            let (min, max) = minMaxValues2D s
            let pMin = pixel i (normalize scalingFactor offset (float min))
            let pMax = pixel i (normalize scalingFactor offset (float max))
            let zero = pixel i (normalize scalingFactor offset 0.0)
            (zero, pMin, pMax)
        )
        |> Seq.fold (fun c (z, p1, p2) -> 
            updateConsole c
            Drawille.drawLine z p1 c
            |> Drawille.drawLine z p2
            ) canvas

    let pointFolder c (z, p1, p2) =
        updateConsoleDiff c (fun nextCanvas ->
            Drawille.drawLine z p1 nextCanvas
            |> Drawille.drawLine z p2)

    let parallelMinMax canvas scalingFactor offset seq = 
        seq
        |> Seq.map (fun (i, samples) -> 
            //printfn "%i" i
            //printfn "min %i, max %i" min max
            let (min, max) = minMaxValues2D samples
            let pMin = pixel i (normalize scalingFactor offset (float min))
            let pMax = pixel i (normalize scalingFactor offset (float max))
            let zero = pixel i (normalize scalingFactor offset 0.0)
            (zero, pMin, pMax)
        )
        |> Seq.fold pointFolder canvas

    let traceWaveformSamples canvas (samples: int[,]) =
        
        let scalingFactor = (pown 2 16) / canvas.Height
        let offset = int canvas.Height / 2

        samples
            |> averageChannels
            |> List.splitInto (min (int canvas.Width) (samples |> Array2D.length2))
            |> List.mapi (fun i s -> pixel i (List.average s |> normalize scalingFactor offset))
            //|> Util.iterTrans (fun p -> printfn "%A" p)
            |> List.pairwise
            |> List.fold (fun c (p1, p2) -> drawLine p1 p2 c) canvas

    let drawRect w h = 

        let top = [0..w-1] |> List.map (fun i -> pixel i 0)
        let bottom = [0..w-1] |> List.map (fun i -> pixel i (h-1))
        let left = [0..h-1] |> List.map (fun i -> pixel 0 i)
        let right = [0..h-1] |> List.map (fun i -> pixel (w-1) i)

        let drawPoints points canvas =
            points 
            |> List.fold (fun c p -> set p c) canvas

        createPixelCanvas w h
        |> drawPoints top
        |> drawPoints bottom
        |> drawPoints left
        |> drawPoints right
        |> toStrings
        |> Seq.reduce (+)
