using Microsoft.Win32.SafeHandles;
using System;
using System.Runtime.InteropServices;

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
        private static extern bool WriteConsoleOutput(
            SafeFileHandle hConsoleOutput,
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

        [StructLayout(LayoutKind.Explicit)]
        public struct CharUnion
        {
            [FieldOffset(0)] public char UnicodeChar;
            [FieldOffset(0)] public byte AsciiChar;
        }

        [StructLayout(LayoutKind.Explicit)]
        public struct CharInfo
        {
            [FieldOffset(0)] public CharUnion Char;
            [FieldOffset(2)] public short Attributes;
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

        private static int w = 50;
        private static int h = 50;
        private static char[,] chars = new char[w,h];

        static void Main(string[] args)
        {
            var handle = CreateConsoleScreenBuffer(
                GENERIC_READ | GENERIC_WRITE,
               FILE_SHARE_READ | FILE_SHARE_WRITE,
               IntPtr.Zero,
               CONSOLE_TEXTMODE_BUFFER,
               IntPtr.Zero);

            for (int x = 0; x < w; x++)
            {
                for (int y = 0; y < h; y++)
                {
                    chars[x, y] = 'a';
                }
            }

            ConsoleApi
        }
    }
}
