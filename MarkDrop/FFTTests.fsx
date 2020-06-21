#load "Converters.fs"
#load "Util.fs"
#load "FFT.fs"
#load "FFFT.fs"
#load "WavAudio.fs"

let fftSampleCount = pown 2 11

let toSineWave frequency amplitude t =
    (amplitude * sin(2. * System.Math.PI * frequency * t))

// let input = 
//     [0. ..48000.]
//     |> List.map (toSineWave 100. 1.)
//     |> List.chunkBySize fftSampleCount

// let output = 
//     input.[0]
//     |> List.map (fun i -> System.Numerics.Complex(i, 0.)) 
//     |> List.toArray 
//     |> FFFT.fft

let fileName = @"C:\Dev\MarkDrop\Audio\sine-sweep.wav"
//let fileName = @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav"
//let fileName = @"C:\Dev\MarkDrop\Audio\kicks-sparse.wav"
//let fileName = @"C:\Dev\MarkDrop\Audio\JANICE - b - 1.wav"
let wavHeader = WavAudio.readHeader fileName false
let wavData = WavAudio.readData fileName wavHeader
let sampleInfo = WavAudio.getSampleInfo wavHeader
let max16Bit = 0.5 * pown 2. 16

let fftByteCount = fftSampleCount * sampleInfo.BytesPerMultiChannelSample

let sampleAggregator (samples: int[,]) =
    samples.[0,*]

let sampleNormalizer (samples: int[]) =
    samples |> Array.map (fun i -> float i / max16Bit)

let maxFFT data =
    data
    |> Array.chunkBySize fftByteCount
    |> Array.map (fun a -> WavAudio.bytesToSamples sampleInfo a |> sampleAggregator |> sampleNormalizer)
    |> Array.filter (fun a -> a.Length = fftSampleCount)
    |> Array.map (List.ofArray >> FFT.fftList >> List.max)
    |> Array.max

let maxFFFT data =
    data
    |> Array.chunkBySize fftByteCount
    |> Array.map (fun a -> WavAudio.bytesToSamples sampleInfo a |> sampleAggregator |> sampleNormalizer)
    |> Array.filter (fun a -> a.Length = fftSampleCount)
    |> Array.map (fun samples -> 
        samples
        |> Array.map (fun s -> System.Numerics.Complex(s, 0.))
        |> FFFT.fft 
        |> Array.map (fun c -> sqrt(c.Real**2. + c.Imaginary**2.))

        |> Array.max)
    |> Array.max


maxFFFT wavData