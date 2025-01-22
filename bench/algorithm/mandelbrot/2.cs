using System;
using System.Linq;
using System.Numerics;
using System.Security.Cryptography;
using System.Text;

public class MandelBrot
{
    private static readonly Vector<double> _threshold = new Vector<double>(4);
    public static void Main(string[] args)
    {
        var size = args.Length == 0 ? 200 : int.Parse(args[0]);
        size = (size + 7) / 8 * 8;
        var chunkSize = size / 8;
        var inv = 2.0 / size;
        Console.WriteLine($"P4\n{size} {size}");

        var xloc = new (Vector<double>, Vector<double>)[chunkSize];
        Span<double> array = stackalloc double[8];
        for (var i = 0; i < chunkSize; i++)
        {
            var offset = i * 8;
            for (var j = 0; j < 8; j++)
            {
                array[j] = (offset + j) * inv - 1.5;
            }
            xloc[i] = (new Vector<double>(array.Slice(0, 4)), new Vector<double>(array.Slice(4, 4)));
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
        Console.WriteLine(Convert.ToHexStringLower(hash));
    }

    static byte mbrot8((Vector<double>, Vector<double>) cr, double civ)
    {
        var ci = new Vector<double>(new[] { civ, civ, civ, civ });
        var zr0 = new Vector<double>(0);
        var zr1 = new Vector<double>(0);
        var zi0 = new Vector<double>(0);
        var zi1 = new Vector<double>(0);
        var tr0 = new Vector<double>(0);
        var tr1 = new Vector<double>(0);
        var ti0 = new Vector<double>(0);
        var ti1 = new Vector<double>(0);
        var absz0 = new Vector<double>(0);
        var absz1 = new Vector<double>(0);
        for (var _i = 0; _i < 10; _i++)
        {
            for (var _j = 0; _j < 5; _j++)
            {
                var tmp = (zr0 + zr0) * zi0;
                zi0 = tmp + ci;
                zr0 = tr0 - ti0 + cr.Item1;

                tr0 = zr0 * zr0;
                ti0 = zi0 * zi0;

                tmp = (zr1 + zr1) * zi1;
                zi1 = tmp + ci;
                zr1 = tr1 - ti1 + cr.Item2;

                tr1 = zr1 * zr1;
                ti1 = zi1 * zi1;
            }
            absz0 = tr0 + ti0;
            absz1 = tr1 + ti1;
            if (Vector.GreaterThanAll(absz0, _threshold) && Vector.GreaterThanAll(absz1, _threshold))
            {
                return 0;
            }
        }

        var accu = (byte)0;
        for (var i = 0; i < 4; i++)
        {
            if (absz0[i] <= 4.0)
            {
                accu |= (byte)(0x80 >> i);
            }
        }
        for (var i = 4; i < 8; i++)
        {
            if (absz1[i - 4] <= 4.0)
            {
                accu |= (byte)(0x80 >> i);
            }
        }
        return accu;
    }
}
