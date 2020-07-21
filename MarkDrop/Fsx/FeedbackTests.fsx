#load "Util.fs"
#load "Converters.fs"
#load "WavAudio.fs"
#load "Drawille.fs"
#load "ConViz.fs"

open System

open Drawille

System.Console.CursorVisible <- false
System.Console.OutputEncoding <- System.Text.Encoding.UTF8
let convas = { ConViz.initialise with ZeroOrigin = pixel 0 20 }
let canvas = Drawille.createCharCanvas convas.CharWidth convas.CharHeight
let origin = Drawille.pixel (int canvas.Width / 2) (int canvas.Height / 2)

let pixels = Array2D.zeroCreate<bool> (int canvas.Width) (int canvas.Height)

let centre origin pixels =
    pixels
    |> Array.map (Drawille.translate origin)

let drawCenterdSquare origin x =
    Drawille.rect {
        A = Drawille.pixel (x/2) (x/2) 
        B = Drawille.pixel (-x/2) (x/2)
        C = Drawille.pixel (-x/2) (-x/2)
        D = Drawille.pixel (x/2) (-x/2)
    }
    |> centre origin

let iterPixels pixels f =
    for x = 0 to Array2D.length1 pixels - 1 do
        for y = 0 to Array2D.length2 pixels - 1 do
            f x y

let pixelsToCanvas pixels canvas =
    iterPixels pixels (fun x y ->
        if pixels.[x,y] then  
            Drawille.set (pixel x y) canvas |> ignore
    )
    canvas

//let initial = drawCenterdSquare origin 10
let initial = [|pixel 0 5|] |> centre origin
initial |> Array.iter (fun p -> pixels.[int p.X, int p.Y] <- true) 
Drawille.drawPoints initial canvas
|> ConViz.updateConsole convas

let rotate angle x y =
    let x = float x * sin angle |> int
    let y = float y * cos angle |> int
    x, y

let fps = 60.
let msPerFrame = 1000. / fps

let rotateOrigin = Drawille.pixel (int canvas.Width / 3) (int canvas.Height / 3)
let angle = Math.PI / 4.

let translateToOrigin x y =
    (x - int origin.X), (y - int origin.Y)

let rotatePixels pixels =

    iterPixels pixels (fun x y -> 
        if pixels.[x,y] then 

            printfn "x: %i y: %i" x y
            pixels.[x,y] <- false
            let tX, tY = translateToOrigin x y
            printfn "tx: %i ty: %i" tX tY
            let newX, newY = rotateCoords angle tX tY
            printfn "newX: %f newY: %f" newX newY
            let finalX, finalY = translateCoords origin (int newX) (int newY)
            printfn "finalX: %i finalY: %i" finalX finalY
            pixels.[finalX,finalY] <- true
    )

printfn "test"

Console.Clear() 

while Console.ReadKey().KeyChar <> 'x' do
//for i = 0 to 1 do

    // iterPixels pixels (fun x y -> 
    //     let (newX, newY) = rotateCoords oneDeg x y
    //     pixels.[x,y] <- false
    //     pixels.[newX,newY] <- true
    // )
    Console.Clear() |> ignore

    rotatePixels pixels

    pixelsToCanvas pixels canvas
    |> ConViz.updateConsole convas
    //Threading.Thread.Sleep(1000)
