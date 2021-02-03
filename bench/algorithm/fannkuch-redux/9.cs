// The Computer Language Benchmarks Game
// https://benchmarksgame-team.pages.debian.net/benchmarksgame/
//
// contributed by Flim Nik
// small optimisations by Anthony Lloyd

using System;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;

public unsafe static class FannkuchRedux
{
    static int taskCount;
    static int[] fact, chkSums, maxFlips;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static void FirstPermutation(short* p, short* pp, int* count, int n, int idx)
    {
        for (int i = 0; i < n; ++i) p[i] = (byte)i;
        for (int i = n - 1; i > 0; --i)
        {
            int d = idx / fact[i];
            count[i] = d;
            if (d > 0)
            {
                idx %= fact[i];
                for (int j = i; j >= 0; --j) pp[j] = p[j];
                for (int j = 0; j <= i; ++j) p[j] = pp[(j + d) % (i + 1)];
            }
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static void NextPermutation(short* p, int* count)
    {
        var first = p[1];
        p[1] = p[0];
        p[0] = first;
        int i = 1;
        while (++count[i] > i)
        {
            count[i++] = 0;
            var next = p[1];
            p[0] = next;
            for (int j = 1; j < i;) p[j] = p[++j];
            p[i] = first;
            first = next;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static void Copy(short* p, short* pp, int n)
    {
        var startL = (long*)p;
        var stateL = (long*)pp;
        var lengthL = n / 4;
        int i = 0;
        for (; i < lengthL; i++)
        {
            stateL[i] = startL[i];
        }
        for (i = lengthL * 4; i < n; i++)
        {
            pp[i] = p[i];
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static int CountFlips(short* p, short* pp, int n)
    {
        int flips = 1;
        int first = *p;
        short temp;
        if (p[first] != 0)
        {
            Copy(p, pp, n);
            do
            {
                ++flips;
                if (first > 2)
                {
                    short* lo = pp + 1, hi = pp + first - 1;
                    do
                    {
                        temp = *lo;
                        *lo = *hi;
                        *hi = temp;
                    } while (++lo < --hi);
                }
                temp = pp[first];
                pp[first] = (short)first;
                first = temp;
            } while (pp[first] != 0);
        }
        return flips;
    }

    static void Run(int n, int taskSize)
    {
        int* count = stackalloc int[n];
        int taskId, chksum = 0, maxflips = 0;
        short* p = stackalloc short[n];
        short* pp = stackalloc short[n];
        while ((taskId = Interlocked.Decrement(ref taskCount)) >= 0)
        {
            FirstPermutation(p, pp, count, n, taskId * taskSize);
            if (*p != 0)
            {
                var flips = CountFlips(p, pp, n);
                chksum += flips;
                if (flips > maxflips) maxflips = flips;
            }
            for (int i = 1; i < taskSize; i++)
            {
                NextPermutation(p, count);
                if (*p != 0)
                {
                    var flips = CountFlips(p, pp, n);
                    chksum += (1 - (i & 1) * 2) * flips;
                    if (flips > maxflips) maxflips = flips;
                }
            }
        }
        chkSums[-taskId - 1] = chksum;
        maxFlips[-taskId - 1] = maxflips;
    }

    public static void Main(string[] args)
    {
        int n = args.Length > 0 ? int.Parse(args[0]) : 7;
        fact = new int[n + 1];
        fact[0] = 1;

        for (int i = 1; i < fact.Length; i++)
        {
            fact[i] = fact[i - 1] * i;
        }

        var PC = Environment.ProcessorCount;
        taskCount = n > 11 ? fact[n] / (9 * 8 * 7 * 6 * 5 * 4 * 3 * 2) : PC;
        int taskSize = fact[n] / taskCount;
        chkSums = new int[PC];
        maxFlips = new int[PC];
        var threads = new Thread[PC];
        for (int i = 1; i < PC; i++)
        {
            (threads[i] = new Thread(() => Run(n, taskSize))).Start();
        }
        Run(n, taskSize);

        for (int i = 1; i < threads.Length; i++)
        {
            threads[i].Join();
        }
        Console.WriteLine(chkSums.Sum() + "\nPfannkuchen(" + n + ") = " + maxFlips.Max());
    }
}