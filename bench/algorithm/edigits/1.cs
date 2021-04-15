using System;
using System.Numerics;

class DigitsOfE
{
    public static void Main(string[] args)
    {
        int n;
        if (args.Length == 0 || !int.TryParse(args[0], out n))
        {
            n = 27;
        }

        var k = BinarySearch(n);
        var (p, q) = SumTerms(0, k - 1);
        p += q;
        var a = BigInteger.Pow(new BigInteger(10), n - 1);
        var answer = p * a / q;
        var answerStr = answer.ToString();
        for (var i = 0; i < n; i += 10)
        {
            Span<char> sb = stackalloc char[10] { ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', };
            for (var j = i; j < n && j < i + 10; j++)
            {
                sb[j - i] = answerStr[j];
            }
            var count = i + 10;
            if (count > n)
            {
                count = n;
            }
            Console.WriteLine($"{new String(sb)}\t:{count}");
        }
    }

    static (BigInteger, BigInteger) SumTerms(int a, int b)
    {
        if (b == a + 1)
        {
            return (new BigInteger(1), new BigInteger(b));
        }
        var mid = (a + b) / 2;
        var (pLeft, qLeft) = SumTerms(a, mid);
        var (pRight, qRight) = SumTerms(mid, b);
        return (pLeft * qRight + pRight, qLeft * qRight);
    }

    static int BinarySearch(int n)
    {
        var a = 0;
        var b = 1;
        while (!TestK(n, b))
        {
            a = b;
            b *= 2;
        }
        while (b - a > 1)
        {
            var m = (a + b) / 2;
            if (TestK(n, m))
            {
                b = m;
            }
            else
            {
                a = m;
            }
        }
        return b;
    }

    static bool TestK(int n, int k)
    {
        if (k < 0)
        {
            return false;
        }
        var lnKFactorial = k * (Math.Log((double)k) - 1) + 0.5 * Math.Log(Math.PI * 2);
        var log10KFactorial = lnKFactorial / Math.Log(10);
        return log10KFactorial >= (double)(n + 50);
    }
}
