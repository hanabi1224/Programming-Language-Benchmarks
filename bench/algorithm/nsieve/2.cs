using System;
using System.Collections;
using System.IO;

internal static class Program
{
    public static void Main(string[] args)
    {
        var n = args.Length > 0 ? int.Parse(args[0]) : 4;
        for (var i = 0; i < 3; i++)
        {
            Sieve(10000 << (n - i));
        }
    }

    private static void Sieve(int n)
    {
        var flags = new BitArray(n, false);
        var count = 0;
        for (var i = 2; i < n; i++)
        {
            if (!flags[i])
            {
                count += 1;
            }
            for (var j = i << 1; j < n; j += i)
            {
                flags[j] = true;
            }
        }
        Console.WriteLine($"Primes up to {n,8} {count,8}");
    }
}
