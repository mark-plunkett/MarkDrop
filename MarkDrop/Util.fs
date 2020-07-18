module Util

    let flip f a b =
        f b a

    let iterTrans f items =
        items |> List.iter f
        items

    let iterTransSeq f items =
        items |> Seq.iter f
        items

    let tee f x =
        f x
        x

    let rec furthestFromZero furthest list =
        match list with
        | head :: tail -> 
            let candidate = abs head
            if candidate > abs furthest then furthestFromZero head tail
            else furthestFromZero furthest tail
        | [] -> furthest

    let unique list =
        list
        |> List.fold (fun acc e ->
            match acc with
            | x::xs when x = e -> acc
            | _ -> e::acc) []
        |> List.rev

    let chunkBytes blockSize bytes =
        match Array.length bytes with
        | length when length < blockSize -> 
            let zeroed = Array.zeroCreate blockSize
            Array.blit bytes 0 zeroed 0 length
            (zeroed, [||])
        | _ -> Array.splitAt blockSize bytes

    let pad size array =
        let zeroed = Array.zeroCreate size
        Array.blit array 0 zeroed 0 (Array.length array)
        zeroed

    let normalize factor (vs: int[]) =
        vs |> Array.map (fun i -> float i / factor)


    let logScale maxValue value =
        let logF = System.Math.Log
        let divisor = logF <| (float maxValue)
        (maxValue * logF (max 1. value)) / divisor