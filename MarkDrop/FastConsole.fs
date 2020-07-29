module FastConsole

    open System
    open System.Runtime.InteropServices

    module private Internal =

        type GenericAccessModes =
        | GENERIC_READ = 0x80000000
        | GENERIC_WRITE = 0x40000000

        [<Struct>]
        [<StructLayout(LayoutKind.Sequential)>]
        type Coord = {
            X : int16
            Y : int16
        }

        [<Struct>]
        [<StructLayout(LayoutKind.Explicit, CharSet = CharSet.Unicode)>]
        type CharInfo =
            [<FieldOffset(0)>]
            val mutable UnicodeChar : int16
            [<FieldOffset(0)>]
            val mutable AsciiChar : int16
            [<FieldOffset(2)>]
            val mutable Attributes : int16

        [<Struct>]
        [<StructLayout(LayoutKind.Sequential)>]
        type SmallRect = {
            Left : int16
            Top : int16
            Right : int16
            Bottom : int16
        }

        [<DllImport(@"kernel32.dll")>]
        extern IntPtr CreateConsoleScreenBuffer(
            uint32 dwDesiredAccess,
            uint32 dwShareMode,
            IntPtr secutiryAttributes,
            uint32 flags,
            IntPtr screenBufferData)

        [<DllImport(@"kernel32.dll", SetLastError = true)>]
        extern bool SetConsoleActiveScreenBuffer(IntPtr handle);

        [<DllImport(@"kernel32.dll", SetLastError = true)>]
        extern bool WriteConsoleOutputW(
            IntPtr hConsoleOutput,
            CharInfo[] lpBuffer,
            Coord dwBufferSize,
            Coord dwBufferCoord,
            SmallRect& lpWriteRegion)

    type FastConsole(width : int, height : int) =

        let bufferSize = { Internal.Coord.X = int16 width; Internal.Coord.Y = int16 height }
        let topLeft = { Internal.Coord.X = int16 0; Internal.Coord.Y = int16 0 }
        let mutable writeRegion = {
            Internal.SmallRect.Top = int16 0
            Internal.SmallRect.Left = int16 0
            Internal.SmallRect.Bottom = int16 height - int16 1
            Internal.SmallRect.Right = int16 width - int16 1
        }

        let whiteTextBlackBackground = 7
        let chars = Array.init (int width * int height) (fun i -> 
            let mutable char = Internal.CharInfo()
            char.UnicodeChar <- int16 0
            char.Attributes <- int16 whiteTextBlackBackground
            char
        )

        let access = uint32 (Internal.GenericAccessModes.GENERIC_READ ||| Internal.GenericAccessModes.GENERIC_WRITE)
        let consoleHandle = Internal.CreateConsoleScreenBuffer(access, uint32 0, IntPtr.Zero, uint32 1, IntPtr.Zero)

        do Internal.SetConsoleActiveScreenBuffer(consoleHandle) |> ignore

        member __.WriteChars (charArray: int16[,]) = 
            
            for x = 0 to Array2D.length1 charArray - 1 do
                for y = 0 to Array2D.length2 charArray - 1 do
                    let index = x + (y * Array2D.length1 charArray)
                    chars.[index].UnicodeChar <- charArray.[x,y] 

            Internal.WriteConsoleOutputW(consoleHandle, chars, bufferSize, topLeft, &writeRegion) |> ignore