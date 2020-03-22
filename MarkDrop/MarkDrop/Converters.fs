module Convert

    let toInt16 x = System.BitConverter.ToInt16(x, 0)
    let toInt32 x = System.BitConverter.ToInt32(x, 0)