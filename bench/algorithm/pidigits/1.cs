using System;
using System.Numerics;

static class Program
{
    static void Main(string[] args)
    {
        var digitsToPrint = args.Length > 0 ? int.Parse(args[0]) : 27;
        var digitsPrinted = 0;

        var k = new BigInteger(1);
        var n1 = new BigInteger(4);
        var n2 = new BigInteger(3);
        var d = new BigInteger(1);

        BigInteger u, v, w;

        while (true)
        {
            u = n1 / d;
            v = n2 / d;

            if (u == v)
            {
                Console.Write(u);
                digitsPrinted += 1;
                var digitsPrintedModTen = digitsPrinted % 10;
                if (digitsPrintedModTen == 0)
                {
                    Console.WriteLine($"\t:{digitsPrinted}");
                }

                if (digitsPrinted >= digitsToPrint)
                {
                    if (digitsPrintedModTen > 0)
                    {
                        for (var i = 0; i < 10 - digitsPrintedModTen; i++)
                        {
                            Console.Write(' ');
                        }
                        Console.WriteLine($"\t:{digitsPrinted}");
                    }

                    return;
                }

                var toMinus = u * 10 * d;
                n1 = n1 * 10 - toMinus;
                n2 = n2 * 10 - toMinus;
            }
            else
            {
                var k2 = k * 2;
                u = n1 * (k2 - 1);
                v = n2 * 2;
                w = n1 * (k - 1);
                n1 = u + v;
                u = n2 * (k + 2);
                n2 = w + u;
                d = d * (k2 + 1);
                k = k + 1;
            }
        }
    }
}
