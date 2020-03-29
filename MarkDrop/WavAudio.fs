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

    let streamData fileName header =
        seq {
            use fileStream = System.IO.File.OpenRead(fileName)
            let dataOffset = (header.ChunkSize - header.SubChunk2Size + header.SubChunk1Size - header.ChunkHeaderConstBytes)
            printfn "Reading data from offset: %i" dataOffset
            let bytesPerSample = header.BitsPerSample / 8
            printfn "Bytes per sample: %i" bytesPerSample

            let bytesPerStereoSample = bytesPerSample * 2

            use binaryReader = new IO.BinaryReader(fileStream, Text.Encoding.Default, false)
            // each sample is made up of 4 bytes:
            //  first two bytes = left channel
            //  last two bytes = right channel

            let buffer:byte[] = Array.zeroCreate (2048 * 1000)
            let mutable bytesRead = binaryReader.Read(buffer, 0, buffer.Length)

            printfn "Read bytes: %i" bytesRead
            while bytesRead > 0 do
                for index in [0..((bytesRead / bytesPerSample) - bytesPerStereoSample)] do
                    let offset = index * bytesPerSample
                    yield {
                        AmpL = buffer.[offset..(offset + bytesPerSample - 1)] |> Convert.to32BitInt16
                        AmpR = buffer.[(offset + bytesPerSample)..(offset + bytesPerSample + bytesPerSample - 1)] |> Convert.to32BitInt16
                    }

                bytesRead <- binaryReader.Read(buffer, 0, buffer.Length)
        }

    let printInfo fileName header = 
        printfn """
File: %s
%A
"""
            fileName
            header