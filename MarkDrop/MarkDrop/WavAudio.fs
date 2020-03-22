module WavAudio

    open System

    let headerSize = 42

    type WavFile = {
        ChunkID: string
        ChunkSize: int
        Format: string

        SubChunk1ID: string
        SubChunk1Size: int
        AudioFormat: int16
        NumChannels: int16
        SampleRate: int
        ByteRate: int
        BlockAlign: int16
        BitsPerSample: int16

        SubChunk2ID: string
        SubChunk2Size: int
    }

    let readWav fileStream =
        use reader = new IO.BinaryReader(fileStream)
        {
            ChunkID = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            ChunkSize = reader.ReadBytes(4) |> Convert.toInt32
            Format = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            SubChunk1ID = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            SubChunk1Size = reader.ReadBytes(4) |> Convert.toInt32
            AudioFormat = reader.ReadBytes(2) |> Convert.toInt16
            NumChannels = reader.ReadBytes(2) |> Convert.toInt16
            SampleRate = reader.ReadBytes(4) |> Convert.toInt32
            ByteRate = reader.ReadBytes(4) |> Convert.toInt32
            BlockAlign = reader.ReadBytes(2) |> Convert.toInt16
            BitsPerSample = reader.ReadBytes(2) |> Convert.toInt16
            SubChunk2ID = reader.ReadBytes(4) |> Text.Encoding.ASCII.GetString
            SubChunk2Size = reader.ReadBytes(4) |> Convert.toInt32
        }

    let printInfo fileName wav = 
        printf """
File: %s
%A
        """
            fileName
            wav