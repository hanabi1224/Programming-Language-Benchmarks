using System;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

public class MandelBrot
{
    public static void Main(string[] args)
    {
        var size = args.Length == 0 ? 200 : int.Parse(args[0]);
        size = (size + 7) / 8 * 8;
        var chunkSize = size / 8;
        var inv = 2.0 / size;
        Console.WriteLine($"P4\n{size} {size}");

        var xloc = new double[chunkSize][];
        for (var i = 0; i < chunkSize; i++)
        {
            var array = new double[8];
            var offset = i * 8;
            for (var j = 0; j < 8; j++)
            {
                array[j] = (offset + j) * inv - 1.5;
            }
            xloc[i] = array;
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

    static byte mbrot8(double[] cr, double civ)
    {
        Span<double> ci = stackalloc double[] { civ, civ, civ, civ, civ, civ, civ, civ };
        Span<double> zr = stackalloc double[8];
        Span<double> zi = stackalloc double[8];
        Span<double> tr = stackalloc double[8];
        Span<double> ti = stackalloc double[8];
        Span<double> absz = stackalloc double[8];
        Span<double> tmp = stackalloc double[8];
        for (var _i = 0; _i < 10; _i++)
        {
            for (var _j = 0; _j < 5; _j++)
            {
                add(zr, zr, tmp);
                mul(tmp, zi, tmp);
                add(tmp, ci, zi);

                minus(tr, ti, tmp);
                add(tmp, cr, zr);

                mul(zr, zr, tr);
                mul(zi, zi, ti);
            }
            add(tr, ti, absz);
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

    static void add(Span<double> a, Span<double> b, Span<double> r)
    {
        for (var i = 0; i < 8; i++)
        {
            r[i] = a[i] + b[i];
        }
    }

    static void minus(Span<double> a, Span<double> b, Span<double> r)
    {
        for (var i = 0; i < 8; i++)
        {
            r[i] = a[i] - b[i];
        }
    }

    static void mul(Span<double> a, Span<double> b, Span<double> r)
    {
        for (var i = 0; i < 8; i++)
        {
            r[i] = a[i] * b[i];
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
