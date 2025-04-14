using System;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

public class MandelBrot
{
    internal unsafe struct F64x8
    {
        internal fixed double buffer[8];

        public F64x8() { }

        public F64x8(double value)
        {
            for (var i = 0; i < 8; i++)
            {
                buffer[i] = value;
            }
        }

        public double Get(int i)
        {
            return buffer[i];
        }

        public void Set(int i, double v)
        {
            buffer[i] = v;
        }

        public static void Add(double* a, double* b, double* r)
        {
            for (var i = 0; i < 8; i++)
            {
                r[i] = a[i] + b[i];
            }
        }

        public static void Minus(double* a, double* b, double* r)
        {
            for (var i = 0; i < 8; i++)
            {
                r[i] = a[i] - b[i];
            }
        }

        public static void Mul(double* a, double* b, double* r)
        {
            for (var i = 0; i < 8; i++)
            {
                r[i] = a[i] * b[i];
            }
        }
    }

    public static void Main(string[] args)
    {
        var size = args.Length == 0 ? 200 : int.Parse(args[0]);
        size = (size + 7) / 8 * 8;
        var chunkSize = size / 8;
        var inv = 2.0 / size;

        var xloc = new F64x8[chunkSize];
        for (var i = 0; i < chunkSize; i++)
        {
            var array = new F64x8();
            var offset = i * 8;
            for (var j = 0; j < 8; j++)
            {
                array.Set(j, (offset + j) * inv - 1.5);
            }
            xloc[i] = array;
        }

        Console.WriteLine($"P4\n{size} {size}");

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

    unsafe static byte mbrot8(F64x8 cr, double civ)
    {
        var ci = new F64x8(civ);
        F64x8 zr;
        F64x8 zi;
        F64x8 tr;
        F64x8 ti;
        F64x8 absz;
        F64x8 tmp;
        for (var _i = 0; _i < 10; _i++)
        {
            for (var _j = 0; _j < 5; _j++)
            {
                F64x8.Add(zr.buffer, zr.buffer, tmp.buffer);
                F64x8.Mul(tmp.buffer, zi.buffer, tmp.buffer);
                F64x8.Add(tmp.buffer, ci.buffer, zi.buffer);

                F64x8.Minus(tr.buffer, ti.buffer, tmp.buffer);
                F64x8.Add(tmp.buffer, cr.buffer, zr.buffer);

                F64x8.Mul(zr.buffer, zr.buffer, tr.buffer);
                F64x8.Mul(zi.buffer, zi.buffer, ti.buffer);
            }
            F64x8.Add(tr.buffer, ti.buffer, absz.buffer);
            var terminate = true;
            for (var i = 0; i < 8; i++)
            {
                if (absz.Get(i) <= 4.0)
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
            if (absz.Get(i) <= 4.0)
            {
                accu |= (byte)(0x80 >> i);
            }
        }
        return accu;
    }
}
