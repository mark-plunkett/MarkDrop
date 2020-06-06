module FFT

    open System
    open System.Numerics

    // this file: http://www.fssnip.net/dC/title/fast-Fourier-transforms-FFT-
    // optimized: http://www.fssnip.net/7Tn/title/Optimized-Fast-Fourier-Transform-FFT
    
    let rec fft = function
      | []  -> []
      | [x] -> [x] 
      | x ->
        x
        |> List.mapi (fun i c -> i % 2 = 0, c)
        |> List.partition fst
        |> fun (even, odd) -> fft (List.map snd even), fft (List.map snd odd)
        ||> List.mapi2 (fun i even odd -> 
            let btf = odd * Complex.FromPolarCoordinates(1., -2. * Math.PI * (float i / float x.Length ))
            even + btf, even - btf)
        |> List.unzip
        ||> List.append
    
    let fftList input = 
        input
        |> List.map (fun r -> Complex(r, 0.)) 
        |> fft
        |> List.take (List.length input / 2)
        |> List.map (fun c -> c.Real)

    // let naudioFFT input m =

    //     let mutable data = input |> Array.map (fun i -> 
    //         let mutable t = NAudio.Dsp.Complex()
    //         t.X <- i
    //         t.Y <- 0. |> float32
    //         t
    //     )
    //     NAudio.Dsp.FastFourierTransform.FFT(true, m, data)

    //     data
    //     |> Array.take (Array.length input / 2)
    //     |> Array.map (fun c -> c.X |> float)