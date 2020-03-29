module Convert

    let to32BitInt16 x = int(System.BitConverter.ToInt16(x, 0))
    let toInt32 x = System.BitConverter.ToInt32(x, 0)