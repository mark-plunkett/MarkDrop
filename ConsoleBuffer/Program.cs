using Microsoft.Win32.SafeHandles;
using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Timers;

namespace ConsoleBuffer
{
    class Program
    {
        [DllImport("kernel32.dll")]
        static extern IntPtr CreateConsoleScreenBuffer(
             uint dwDesiredAccess,
             uint dwShareMode,
             IntPtr secutiryAttributes,
             uint flags,
             IntPtr screenBufferData
             );

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool SetConsoleActiveScreenBuffer(IntPtr handle);

        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool WriteConsoleOutputW(
            IntPtr hConsoleOutput,
            CharInfo[] lpBuffer,
            Coord dwBufferSize,
            Coord dwBufferCoord,
            ref SmallRect lpWriteRegion);

        [StructLayout(LayoutKind.Sequential)]
        public struct Coord
        {
            public short X;
            public short Y;

            public Coord(short X, short Y)
            {
                this.X = X;
                this.Y = Y;
            }
        };

        [StructLayout(LayoutKind.Explicit, CharSet = CharSet.Unicode)]
        public struct CharInfo
        {
            [FieldOffset(0)] public char UnicodeChar;
            [FieldOffset(0)] public char AsciiChar;
            [FieldOffset(2)] public UInt16 Attributes;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct SmallRect
        {
            public short Left;
            public short Top;
            public short Right;
            public short Bottom;
        }

        public const uint GENERIC_READ = (0x80000000);
        public const uint GENERIC_WRITE = (0x40000000);

        public const uint FILE_SHARE_READ = 0x00000001;
        public const uint FILE_SHARE_WRITE = 0x00000002;

        public const uint CONSOLE_TEXTMODE_BUFFER = 1;

        private static short w = 10;
        private static short h = 20;
        private static Coord bufferSize = new Coord(w, h);
        private static Coord topLeft = new Coord(0, 0);
        private static SmallRect writeRegion = new SmallRect
        {
            Top = 0,
            Left = 0,
            Bottom = (short)(h - 1),
            Right = (short)(w - 1)
        };
        private static CharInfo[] chars = new CharInfo[w * h];

        static IntPtr handle;
        static ushort attributes = 7;

        static void Main(string[] args)
        {
            //InitialiseChars();
            Console.OutputEncoding = System.Text.Encoding.UTF8;

            handle = CreateConsoleScreenBuffer(
                GENERIC_READ | GENERIC_WRITE,
                0,
                IntPtr.Zero,
                CONSOLE_TEXTMODE_BUFFER,
                IntPtr.Zero);

            SetConsoleActiveScreenBuffer(handle);

            while (true)
            {
                WriteChars();
                //Thread.Sleep(500);
                Console.ReadKey();
            }

            int t = 0;
            //Marshal.FreeHGlobal(flattened);
        }

        private static void WriteChars()
        {
            for (short i = 0; i < w * h; i++)
            {
                chars[i] = new CharInfo { UnicodeChar = (char)0x28FF, Attributes = 7 };
            }

            WriteConsoleOutputW(handle, chars, bufferSize, topLeft, ref writeRegion);

            attributes++;
        }
    }
}
