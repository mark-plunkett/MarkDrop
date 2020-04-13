module EQViz

    open Util
    open Drawille

    let bands = [
        32
        64
        125
        250
        500
        1000
        2000
        4000
        8000
        16000
    ]

    // TODO: flip y axis - https://stackoverflow.com/questions/1485745/flip-coordinates-when-drawing-to-control
    // perhaps implement general translation for entire canvas?
    let drawEQ (bandData:Map<int, int>) canvas = 
        let bandWidth = int canvas.Width / ((List.length bands) + 1)
        let bandRects = 
            [0..List.length bands - 1]
            |> List.map (fun i -> 
                let data = bandData.[i]
                let xOffset = i * (bandWidth + 1)
                {
                    Drawille.Shapes.Rect.A = pixel xOffset data
                    Drawille.Shapes.Rect.B = pixel (xOffset + bandWidth) data
                    Drawille.Shapes.Rect.C = pixel (xOffset + bandWidth) 0
                    Drawille.Shapes.Rect.D = pixel xOffset 0        
                })

        List.fold (flip Drawille.drawRect) canvas bandRects