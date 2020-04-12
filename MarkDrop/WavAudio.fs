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
    // The array is chunked into slices of length numSamples
    let processData fileName numSamples =

        seq {
        
            let header = readHeader fileName false
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

    let calculateLength bytes offset length =
        if offset + length > Array.length bytes then 
            Array.length bytes - offset
        else
            length

    let readBytes bytes offset length =
        let maxLength = calculateLength bytes offset length
        bytes.[offset..offset + maxLength - 1]

    let readData fileName header =
        System.IO.File.ReadAllBytes(fileName).[44..44 + header.SubChunk2Size - 1]

    let bytesToSamples (header:WavFileHeader) sampleInfo byteBuffer =
        let multiChannelSamples = Array2D.zeroCreate header.NumChannels ((byteBuffer |> Array.length) / sampleInfo.BytesPerMultiChannelSample)
        let mutable index = 0
        while index < (Array.length byteBuffer / sampleInfo.BytesPerMultiChannelSample) do
            let mutable channel = 0
            while channel < header.NumChannels do

                let sampleOffset = (index * sampleInfo.BytesPerMultiChannelSample)
                let channelOffset = sampleOffset + (channel * 2)
                let sample = Array.sub byteBuffer channelOffset sampleInfo.BytesPerSample |> Convert.to32BitInt16
                Array2D.set multiChannelSamples channel index sample     
                channel <- channel + 1
                
            index <- index + 1

        multiChannelSamples

    let processAllData fileName samplesPerChunk =
        
        let header = readHeader fileName true
        let sampleInfo = getSampleInfo header        
        let bufferLengthBytes = min header.SubChunk2Size (samplesPerChunk * sampleInfo.BytesPerMultiChannelSample)

        let dataBytes = readData fileName header
        let mutable byteBuffer = readBytes dataBytes 0 bufferLengthBytes
        let mutable bytesRead = Array.length byteBuffer
        
        seq {

            while Array.length byteBuffer > 0 do
                
                yield bytesToSamples header sampleInfo byteBuffer
        
                byteBuffer <- readBytes dataBytes (bytesRead + Array.length byteBuffer) bufferLengthBytes
                bytesRead <- bytesRead + Array.length byteBuffer
        }

    let parallelProcessAllData fileName samplesPerChunk =

        let header = readHeader fileName true
        let sampleInfo = getSampleInfo header        
        let bytesPerChunk = samplesPerChunk * sampleInfo.BytesPerMultiChannelSample
        let dataBytes = readData fileName header

        if bytesPerChunk >= header.SubChunk2Size then
            dataBytes
            |> Array.chunkBySize sampleInfo.BytesPerMultiChannelSample
            |> Array.mapi (fun i sampleBytes -> (i, bytesToSamples header sampleInfo sampleBytes))
        else 
            dataBytes
            |> Array.chunkBySize bytesPerChunk
            |> Array.Parallel.mapi (fun i sampleBytes -> (i, bytesToSamples header sampleInfo sampleBytes))

    let printInfo fileName header = 
        printfn """
File: %s
%A
"""
            fileName
            header