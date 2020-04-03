module Util

    let flip f a b =
        f b a

    let iterTrans f items =
        items |> List.iter f
        items

    let iterTransSeq f items =
        items |> Seq.iter f
        items

    let rec furthestFromZero furthest list =
        match list with
        | head :: tail -> 
            let candidate = abs head
            if candidate > abs furthest then furthestFromZero head tail
            else furthestFromZero furthest tail
        | [] -> furthest