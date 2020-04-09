module WavAudio

    open System

    type WavFileHeader = {
        ChunkHeaderConstBytes: int
        ChunkID: string
        ChunkSize: int
        Format: string

        SubChunk1ID: string
        SubChunk1Size: int
        AudioFormat: int
        NumChannels: int
        SampleRate: int
        ByteRate: int
        BlockAlign: int
        BitsPerSample: int

        SubChunk2ID: string
        SubChunk2Size: int
    }

    type StereoSample = {
        AmpL: int
        AmpR: int
    }

    type Sample = {
        Depth: int
        Value: int
    }

    type SampleInfo = {
        BytesPerSample: int
        NumChannels: int
        BytesPerMultiChannelSample: int
        SampleRate: int
        NumSamples: int
    }

    let readHeader fileName disposeFileStream =
        use fileStream = System.IO.File.OpenRead(fileName)
        use reader = new IO.BinaryReader(fileStream, Text.Encoding.Default, not disposeFileStream)
        {
            ChunkHeaderConstBytes = 8
            ChunkID = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            ChunkSize = reader.ReadBytes(4) |> Convert.toInt32
            Format = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            SubChunk1ID = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            SubChunk1Size = reader.ReadBytes(4) |> Convert.toInt32
            AudioFormat = reader.ReadBytes(2) |> Convert.to32BitInt16
            NumChannels = reader.ReadBytes(2) |> Convert.to32BitInt16
            SampleRate = reader.ReadBytes(4) |> Convert.toInt32
            ByteRate = reader.ReadBytes(4) |> Convert.toInt32
            BlockAlign = reader.ReadBytes(2) |> Convert.to32BitInt16
            BitsPerSample = reader.ReadBytes(2) |> Convert.to32BitInt16
            SubChunk2ID = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            SubChunk2Size = reader.ReadBytes(4) |> Convert.toInt32
        }

    let getSampleInfo header = 
        let bytesPerSample = header.BitsPerSample / 8
        let bytesPerMultiChannelSample = bytesPerSample * header.NumChannels
        {
            BytesPerSample = bytesPerSample
            NumChannels = header.NumChannels
            BytesPerMultiChannelSample = bytesPerMultiChannelSample
            SampleRate = header.SampleRate
            NumSamples = header.SubChunk2Size / bytesPerMultiChannelSample
        }

    // Returns a 2D array, first dimension is per channel, second dimension is samples for that channel
    // [
    //      [1, 2, 3, ...] // channel 1 samples
    //      [4, 5, 6, ...] // channel 2 samples
    //  ]
    let processData fileName header numSamples =

        seq {
        
            let sampleInfo = getSampleInfo header

            use fileStream = System.IO.File.OpenRead(fileName)
            use binaryReader = new IO.BinaryReader(fileStream, Text.Encoding.Default, false)
            // discard header bytes
            let dataOffset = (header.ChunkSize - header.SubChunk2Size + header.SubChunk1Size - header.ChunkHeaderConstBytes)
            let _ = binaryReader.ReadBytes(dataOffset)
        
            let bufferLengthBytes = min header.SubChunk2Size (numSamples * sampleInfo.BytesPerMultiChannelSample)
            // e.g 1000 samples * (2 bytes per sample * 2 channels) = 4000 bytes
            let buffer:byte[] = Array.zeroCreate bufferLengthBytes
            let mutable bytesRead = binaryReader.Read(buffer, 0, bufferLengthBytes)
            let multiChannelSamples = Array2D.zeroCreate header.NumChannels (bufferLengthBytes / sampleInfo.BytesPerMultiChannelSample)
            
            printfn "Read bytes: %i" bytesRead
            while bytesRead > 0 do
                // e.g. 0 .. (4000 / 2) - 4 = 1996
                for index in [0..((bytesRead / sampleInfo.BytesPerMultiChannelSample) - sampleInfo.BytesPerMultiChannelSample)] do
                    for channel in [0..header.NumChannels - 1] do
                        // e.g. 
                        // 0 x 0 x 2 = 0
                        let offset = (index * sampleInfo.BytesPerMultiChannelSample)
                        let channelOffset = offset + (channel + 1) * sampleInfo.BytesPerSample
                        let sample = buffer.[channelOffset..channelOffset + sampleInfo.BytesPerSample - 1] |> Convert.to32BitInt16
                        Array2D.set multiChannelSamples channel index sample
            
                //Array2D.fold processor initial multiChannelSamples
                yield multiChannelSamples
            
                bytesRead <- binaryReader.Read(buffer, 0, bufferLengthBytes)

        }

    let printInfo fileName header = 
        printfn """
File: %s
%A
"""
            fileName
            header