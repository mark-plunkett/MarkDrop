#r @"C:\Users\markj\.nuget\packages\fsharpx.collections\2.1.2\lib\netstandard2.0\FSharpx.Collections.dll"
#r @"C:\Users\markj\.nuget\packages\fsharp.collections.parallelseq\1.1.2\lib\netstandard2.0\FSharp.Collections.ParallelSeq.dll"

#load "Converters.fs"
#load "Util.fs"
#load "FFT.fs"
#load "FFFT.fs"
#load "Drawille.fs"
#load "WavAudio.fs"
#load "ConViz.fs"
#load "WaveformViz.fs"

open FSharpx.Collections
open Drawille
open Drawille.Shapes

(*
    Need to read audio stream and process it at a rate defined by SampleInfo.SampleRate
    Process will be 
        
        read x samples, where x is some power of 2
        
        do fft

        update vizualization

*)

try
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    System.Console.CursorVisible <- false
with
    _ -> ()

let canvasWidth = 360

let canvas = createPixelCanvas canvasWidth 160

let fileName = @"D:\Google Drive\Music\flac\Prodigy\The Prodigy - Music For The Jilted Generation (1995) WAV\02. Break & Enter.wav"
let wavHeader = WavAudio.readHeader fileName false
let wavData = WavAudio.readData fileName wavHeader
let sampleInfo = WavAudio.getSampleInfo wavHeader

let numSamples = int (pown 2. 10)

type VizState = {
    ReadOffset: int
}

let initialState = {
    ReadOffset = 0
}

let fftViz (state: ConViz.FrameState) canvas vizState =

    let sampleBytes = Array.sub wavData vizState.ReadOffset numSamples
    let samples = WavAudio.bytesToSamples sampleInfo sampleBytes

    // TODO: this is only using left channel atm
    let output = FFT.fftList (samples.[0,*] |> Array.map float |> List.ofArray)
    
    output
    |> WaveformViz.drawWaveform (canvas |> Drawille.clear)
    |> ConViz.updateConsole

    let nextState = { vizState with ReadOffset = vizState.ReadOffset + numSamples}

    (canvas, nextState)

ConViz.animateState fftViz canvas initialState