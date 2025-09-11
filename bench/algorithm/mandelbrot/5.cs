using System.Runtime.CompilerServices;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;
using System.Security.Cryptography;

namespace mandelbrot;

public static class MandelBrot
{
    public static void Main(string[] args)
    {
        int size = args.Length == 0 ? 200 : int.Parse(args[0]);
        size = (size + 7) / 8 * 8;
        int rowBytes = size / 8;
        double inv = 2.0 / size;

        Console.WriteLine($"P4\n{size} {size}");


        byte[] data = new byte[size * rowBytes];

        // Constants
        Vector256<double> idx4 = Vector256.Create(0.0, 1.0, 2.0, 3.0);
        Vector256<double> four = Vector256.Create(4.0);
        Vector256<double> invV = Vector256.Create(inv);
        Vector256<double> neg1_5 = Vector256.Create(-1.5);

        Parallel.For(0, size, y =>
        {
            double ciScalar = y * inv - 1.0;
            Vector256<double> ci = Vector256.Create(ciScalar);

            for (int xb = 0; xb < rowBytes; xb++)
            {
                double baseX = xb * 8.0;


                Vector256<double> cr0 = Vector256.Add(
                    Vector256.Multiply(
                        Vector256.Add(idx4, Vector256.Create(baseX)), invV),
                    neg1_5);
                Vector256<double> cr1 = Vector256.Add(
                    Vector256.Multiply(
                        Vector256.Add(idx4, Vector256.Create(baseX + 4.0)), invV),
                    neg1_5);

                byte hi = MBrot4Block5Mask(cr0, ci, four, hiNibble: true);
                byte lo = MBrot4Block5Mask(cr1, ci, four, hiNibble: false);
                data[y * rowBytes + xb] = (byte)(hi | lo);
            }
        });
        byte[] hash = MD5.HashData(data.AsSpan());
        Console.WriteLine(Convert.ToHexString(hash.AsSpan()).ToLowerInvariant());
    }


    [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
    private static byte MBrot4Block5Mask( Vector256<double> cr, Vector256<double> ci, Vector256<double> four, bool hiNibble)
    {
        Vector256<double> zr = Vector256<double>.Zero;
        Vector256<double> zi = Vector256<double>.Zero;
        Vector256<double> tr = Vector256<double>.Zero;
        Vector256<double> ti = Vector256<double>.Zero;
        Vector256<double> inside = Vector256<long>.Zero.AsDouble();

        for (int outer = 0; outer < 10; outer++)
        {
            for (int j = 0; j < 5; j++)
            {
                // zi = (zr + zr) * zi + ci;
                Vector256<double> twoZr = Vector256.Add(zr, zr);
                zi = Vector256.Add(Vector256.Multiply(twoZr, zi), ci);

                // zr = tr - ti + cr; (tr, ti from previous sub-iteration)
                zr = Vector256.Add(Vector256.Subtract(tr, ti), cr);

                // tr = zr*zr; ti = zi*zi;
                tr = Vector256.Multiply(zr, zr);
                ti = Vector256.Multiply(zi, zi);
            }

            Vector256<double> abs2 = Vector256.Add(tr, ti);
            inside = Vector256.LessThanOrEqual(abs2, four).AsDouble();
        }

        int m = Avx.MoveMask(inside);

        if (hiNibble)
        {
            return (byte)(
                ((m & 1) << 7) |
                ((m & 2) << 5) |
                ((m & 4) << 3) |
                ((m & 8) << 1)
            );
        }

        return (byte)(
            ((m & 1) << 3) |
            ((m & 2) << 1) |
            ((m & 4) >> 1) |
            ((m & 8) >> 3)
        );
    }
}