open System
open NAudio
open ConViz
(*
Args
    
    -f|--file       Path to file, currently only .wav is supported
    -w|--waveform   Displays the waveform of the input file
    --display-mono  Displays the waveform as a mono sum


*)

type Args = {
    FileName: string
}

let parseArgs args =
    {FileName = ""}

let drawWaveform fileName =

    let wavHeader = WavAudio.readHeader fileName false
    wavHeader |> WavAudio.printInfo fileName
    let sampleInfo = WavAudio.getSampleInfo wavHeader

    let convas = ConViz.initialise
    let canvas = Drawille.createPixelCanvas (convas.CharWidth * 2) (convas.CharHeight * 4)
    let numSamples = wavHeader.SubChunk2Size / sampleInfo.BytesPerMultiChannelSample
    let samplesPerChunk = WaveformViz.getChunkSize numSamples canvas    

    WavAudio.parallelProcessAllData fileName samplesPerChunk
    |> WaveformViz.drawMonoSumWaveform canvas (float Int16.MaxValue)
    |> Drawille.toStrings
    |> printfn "%s"

let startViz (animation: (WavAudio.SampleInfo -> Vizualizer<'TUserState, byte[]>)) fileName =

    let wavHeader = WavAudio.readHeader fileName false
    let sampleInfo = WavAudio.getSampleInfo wavHeader
    let wavData = WavAudio.readData fileName wavHeader
    let viz = animation sampleInfo
    Animation.animate sampleInfo wavData viz.Start

// let animateRect viz =

//     let convas = ConViz.initialise
//     let canvas = Drawille.createCharCanvas convas.CharWidth convas.CharHeight

//     let rectAnimator canvas state =
//         ConViz.rotateRect canvas state
//         |> ConViz.updateConsole convas
//         state.UserState

//     let rectUserStateAggregator oldData newData =
//         match newData with
//         | Some data -> data
//         | None -> oldData

//     let viz = Vizualizer((rectAnimator canvas), rectUserStateAggregator, 1.).Start

//     let rec feed f =

//         viz.Post f 
//         Threading.Thread.Sleep 500
//         feed <| f + 1.

//     feed 1. |> ignore

[<EntryPoint>]
let main argv =

    // resources
    // WAV format: https://web.archive.org/web/20141213140451/https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
    // FFT: https://en.wikipedia.org/wiki/Fast_Fourier_transform
    // Cooley–Tukey FFT: https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

    //let fileName = @"C:\Dev\MarkDrop\Audio\single-sine.wav"
    // !!! 24BIT IS BROKEN
    //let fileName = @"D:\Google Drive\Production\Samples\# Synth Drums\unprocessed drums\toms\unusual toms\wasd_tom_sys100_ceramic-2_s_u.wav"
    // LONG FILE ~1GB
    // let fileName = @"D:\Google Drive\Music\Mixes\Jungle\Gold Dubs Revamped Classics Mix 2014.wav"

    //let fileName = @"C:\Dev\MarkDrop\Audio\imager-flower.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\imager-very-phased.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\imager-left-45.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\test-phased.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\sine-sweep.wav"
    let fileName = @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav"
    //let fileName = @"D:\Google Drive\Music\flac\FC Kahuna\Machine Says Yes\(1) Hayling.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\JANICE - b - 1.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\silence.wav"
    //let fileName = @"C:\Dev\MarkDrop\Audio\kicks-sparse.wav"

    if argv.[0] = "-w" then

        drawWaveform fileName

    else if argv.[0] = "--spectrum" then

        startViz Animation.Spectrum.spectrum fileName

    else if argv.[0] = "--phase" then

        startViz Animation.Phase.phase fileName

    0