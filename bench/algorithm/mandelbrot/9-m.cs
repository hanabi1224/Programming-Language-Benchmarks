/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   started with Java #2 program (Krause/Whipkey/Bennet/AhnTran/Enotus/Stalcup)
   adapted for C# by Jan de Vaan
   simplified and optimised to use TPL by Anthony Lloyd
   optimized to use Vector<double> by Tanner Gooding
   small optimisations by Anthony Lloyd
   modified by Grigory Perepechko
*/

using System;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;

public class MandelBrot
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static unsafe byte GetByte(double* pCrb, double Ciby)
    {
        var res = 0;
        for (var i = 0; i < 8; i += 2)
        {
            var vCrbx = Unsafe.Read<Vector<double>>(pCrb + i);
            var vCiby = new Vector<double>(Ciby);
            var Zr = vCrbx;
            var Zi = vCiby;
            int b = 0, j = 49;
            do
            {
                for (int counter = 0; counter < 7; counter++)
                {
                    var nZr = Zr * Zr - Zi * Zi + vCrbx;
                    var ZrZi = Zr * Zi;
                    Zi = ZrZi + ZrZi + vCiby;
                    Zr = nZr;
                    j--;
                }

                var t = Zr * Zr + Zi * Zi;
                if (t[0] > 4.0) { b |= 2; if (b == 3) break; }
                if (t[1] > 4.0) { b |= 1; if (b == 3) break; }
            } while (j > 0);
            res = (res << 2) + b;
        }
        return (byte)(res ^ -1);
    }
    public static unsafe void Main(string[] args)
    {
        var size = args.Length == 0 ? 200 : int.Parse(args[0]);
        // Ensure image_Width_And_Height are multiples of 8.
        size = (size + 7) / 8 * 8;
        Console.Out.WriteAsync(String.Concat("P4\n", size, " ", size, "\n"));
        var Crb = new double[size + 2];
        var lineLength = size >> 3;
        var data = new byte[size * lineLength];
        fixed (double* pCrb = &Crb[0])
        fixed (byte* pdata = &data[0])
        {
            var value = new Vector<double>(
                  new double[] { 0, 1, 0, 0, 0, 0, 0, 0 }
            );
            var invN = new Vector<double>(2.0 / size);
            var onePtFive = new Vector<double>(1.5);
            var step = new Vector<double>(2);
            for (var i = 0; i < size; i += 2)
            {
                Unsafe.Write(pCrb + i, value * invN - onePtFive);
                value += step;
            }
            var _Crb = pCrb;
            var _pdata = pdata;
            Parallel.For(0, size, y =>
            {
                var Ciby = _Crb[y] + 0.5;
                for (var x = 0; x < lineLength; x++)
                {
                    _pdata[y * lineLength + x] = GetByte(_Crb + x * 8, Ciby);
                }
            });
            using var hasher = MD5.Create();
            var hash = hasher.ComputeHash(data);
            Console.WriteLine(ToHexString(hash));
        }
    }

    static string ToHexString(byte[] ba)
    {
        StringBuilder hex = new StringBuilder(ba.Length * 2);
        foreach (byte b in ba)
        {
            hex.AppendFormat("{0:x2}", b);
        }
        return hex.ToString();
    }
}
