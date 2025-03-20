using System.Runtime.CompilerServices;
using System.Runtime.Intrinsics;
using System.Security.Cryptography;
using System.Text;
using System;

public class MandelBrot
{
    public static void Main(string[] args)
    {
        var size = args.Length == 0 ? 200 : int.Parse(args[0]);
        size = (size + 7) / 8 * 8;
        var chunkSize = size / 8;
        var inv = 2.0 / size;
        Console.WriteLine($"P4\n{size} {size}");

        var xloc = new Vector512<double>[chunkSize];
        for (var i = 0; i < chunkSize; i++)
        {
            var array = new double[8];
            var offset = i * 8;
            for (var j = 0; j < 8; j++)
            {
                array[j] = (offset + j) * inv - 1.5;
            }
            xloc[i] = Vector512.Create(array);
        }

        var data = new byte[size * chunkSize];

        for (var y = 0; y < size; y++)
        {
            var ci = y * inv - 1.0;
            for (var x = 0; x < chunkSize; x++)
            {
                var r = mbrot8(xloc[x], ci);
                if (r > 0)
                {
                    data[y * chunkSize + x] = r;
                }
            }
        }

        using var hasher = MD5.Create();
        var hash = hasher.ComputeHash(data);
        Console.WriteLine(ToHexString(hash));
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static byte mbrot8(Vector512<double> cr, double civ)
    {
        Vector512<double> ci = Vector512.Create(civ);
        Vector512<double> zr = Vector512.Create(0.0);
        Vector512<double> zi = Vector512.Create(0.0);
        Vector512<double> tr = Vector512.Create(0.0);
        Vector512<double> ti = Vector512.Create(0.0);
        Vector512<double> absz = Vector512.Create(0.0);
        
        for (var _i = 0; _i < 10; _i++)
        {
            for (var _j = 0; _j < 5; _j++)
            {
                zi = (zr + zr) * zi + ci;
                zr = tr - ti + cr;
                tr = zr * zr;
                ti = zi * zi;
            }
            absz = tr + ti;
            var terminate = true;
            for (var i = 0; i < 8; i++)
            {
                if (absz[i] <= 4.0)
                {
                    terminate = false;
                    break;
                }
            }
            if (terminate)
            {
                return 0;
            }
        }

        var accu = (byte)0;
        for (var i = 0; i < 8; i++)
        {
            if (absz[i] <= 4.0)
            {
                accu |= (byte)(0x80 >> i);
            }
        }
        return accu;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
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
