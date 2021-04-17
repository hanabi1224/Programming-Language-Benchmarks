// Port from go concurrent prime sieve with goroutines

using System;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;

static class ConcurrentPrimeSieve
{
    const int ChannelSize = 2;
    public static async Task Main(string[] args)
    {
        int n;
        if (args.Length == 0 || !int.TryParse(args[0], out n))
        {
            n = 27;
        }

        using var cts = new CancellationTokenSource();
        var ch = Channel.CreateBounded<int>(ChannelSize);
        _ = GenerateAsync(ch.Writer);
        for (var i = 0; i < n; i++)
        {
            var prime = await ch.Reader.ReadAsync().ConfigureAwait(false);
            Console.WriteLine(prime);
            var chNext = Channel.CreateBounded<int>(ChannelSize);
            _ = FilterAsync(ch.Reader, chNext.Writer, prime, cts.Token);
            ch = chNext;
        }

        cts.Cancel();
    }

    static async Task GenerateAsync(ChannelWriter<int> writer)
    {
        await Task.Yield();
        for (var i = 2; ; i++)
        {
            await writer.WriteAsync(i).ConfigureAwait(false);
        }
    }

    static async Task FilterAsync(
        ChannelReader<int> reader,
        ChannelWriter<int> writer,
        int prime,
        CancellationToken ct)
    {
        await Task.Yield();
        while (!ct.IsCancellationRequested)
        {
            var n = await reader.ReadAsync(ct).ConfigureAwait(false);
            if (n % prime != 0)
            {
                await writer.WriteAsync(n, ct).ConfigureAwait(false);
            }
        }
    }
}
