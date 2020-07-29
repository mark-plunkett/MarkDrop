#load "../FastConsole.fs"

let w = 20
let h = 10

let fastConsole = FastConsole.FastConsole(20, 10)

let alphabet =
    [65..90] @ [97..122]
    |> Array.ofList

let numbers =
    [48..57]
    |> Array.ofList

let r = System.Random()

let chars = Array2D.zeroCreate w h

let enumerate f a =
    let mutable i = 0
    for y = 0 to Array2D.length2 a - 1 do
        for x = 0 to Array2D.length1 a - 1 do
            f i x y
            i <- i + 1

while true do
    
    chars |> enumerate (fun i x y -> 
        let index = i % Array.length numbers
        chars.[x,y] <- numbers.[index] |> char
        //let index = i % Array.length alphabet
        //chars.[x,y] <- alphabet.[index] |> char
    )

    fastConsole.WriteChars(chars) |> ignore
