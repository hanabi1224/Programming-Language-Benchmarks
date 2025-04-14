using System;
using System.Diagnostics;
using System.Globalization;
using System.Numerics;

// change-log:
// 2022-10-22: Don't rely on BigInteger, it's slow as dirt (@jjxtra)

static class Program
{
    static void Main(string[] args)
    {
        var digitsToPrint = args.Length > 0 ? int.Parse(args[0]) : 27;
        var digitsPrinted = 0;
        var piEnum = EnumeratePi((uint)digitsToPrint);
        while (piEnum.MoveNext())
        {
            var u = piEnum.Current;
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
        }
    }

	// note that this enumerator will introduce some overhead but it is good enough without making the code too messy to read
    private static IEnumerator<uint> EnumeratePi(uint n)
    {
        uint LEN = (n / 4 + 1) * 14;
        uint[] a = new uint[LEN];      //array of 4-digit-decimals
        uint b;                        //nominator prev. base
        uint c = LEN;                  //index
        uint d = 0;                    //accumulator and carry
        uint e = 0;                    //save previous 4 digits
        uint f = 10000;                //new base, 4 decimal digits
        uint g;                        //denom previous base
        uint h;
        uint hCopy;

        // initialize state
        for (var idx = 0; idx < a.Length; idx++)
        {
            a[idx] = 2000;
        }

        //outer loop: 4 digits per loop
        for (; (b = c -= 14) != 0;)
        {
            //inner loop: radix conv
            for (; --b > 0;)
            {
                d *= b; // acc *= nom prev base
                d += a[b] * f;
                g = b + b - 1; // denom prev base
                a[b] = d % g;
                d /= g; // save carry
            }

            h = e + d / f;

            // return 0's as if h was a string and padded left with 0's to 4 digits
            hCopy = h;
            hCopy /= 1000;
            yield return hCopy;
            hCopy = h;
            hCopy /= 100;
            yield return hCopy % 10;
            hCopy = h;
            hCopy /= 10;
            yield return hCopy % 10;
            yield return h % 10;

            d = e = d % f; // save currently 4 digits, assure a small enough d
        }
    }
}
