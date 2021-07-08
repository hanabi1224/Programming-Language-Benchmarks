using System;
using System.Collections.Generic;
using System.Threading;

static class ConcurrentPrimeSieve
{
    public static void Main(string[] args)
    {
        int n;
        if (args.Length == 0 || !int.TryParse(args[0], out n))
        {
            n = 27;
        }

        var stream = Generate().GetEnumerator();
        for (var i = 0; i < n; i++)
        {
            stream.MoveNext();
            var prime = stream.Current;
            Console.WriteLine(prime);
            stream = Filter(stream, prime).GetEnumerator();
        }
    }

    static IEnumerable<int> Generate()
    {
        for (var i = 2; ; i++)
        {
            yield return i;
        }
    }

    static IEnumerable<int> Filter(IEnumerator<int> stream, int prime)
    {
        while (stream.MoveNext())
        {
            var i = stream.Current;
            if (i % prime != 0)
            {
                yield return i;
            }
        }
    }
}
