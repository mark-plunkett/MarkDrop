module ConViz

    open Drawille

    let traceWaveForm conWidth conHeight (samples:list<WavAudio.StereoSample>) =
        
        let canvasWidth = (conWidth * 2) - 2
        let canvasHeight = conHeight * 4
        let scalingFactor = (pown 2 16) / (canvasHeight)
        let offset = canvasHeight / 2

        let normalize i = 
            int (float i / float scalingFactor ) + offset

        samples
            |> List.map (fun s -> (s.AmpR + s.AmpL) / 2)
            |> List.splitInto canvasWidth
            //|> List.mapi (fun i s -> point i (List.average s |> normalize))
            |> List.mapi (fun i s -> point i ((Util.furthestFromZero 0 s) |> normalize))
            //|> Util.iterTrans (fun p -> printfn "%i:%i" p.X p.Y)
            |> List.pairwise
            |> List.fold (fun c (p1, p2) -> drawLine p1 p2 c) (createCanvasAbsolute canvasWidth canvasHeight)
            |> toStrings
            |> Seq.reduce (+)
            
    let drawRect w h = 

        let top = [0..w-1] |> List.map (fun i -> (i, 0))
        let bottom = [0..w-1] |> List.map (fun i -> (i, h-1))
        let left = [0..h-1] |> List.map (fun i -> (0, i))
        let right = [0..h-1] |> List.map (fun i -> (w-1, i))

        let drawPoints points canvas =
            points 
            |> List.fold (fun c (x, y) -> set x y c) canvas

        createCanvasAbsolute (w * 2) (h * 2)
        |> drawPoints top
        |> drawPoints bottom
        |> drawPoints left
        |> drawPoints right
        |> toStrings
        |> Seq.reduce (+)
