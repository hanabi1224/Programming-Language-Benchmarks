// Ported from go concurrent prime sieve with goroutines

using System;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;

static class ConcurrentPrimeSieve
{
    private static readonly BoundedChannelOptions s_channelOptions = new BoundedChannelOptions(1)
    {
        SingleWriter = true,
        SingleReader = true,
        AllowSynchronousContinuations = false,
    };

    public static async Task Main(string[] args)
    {
        int n;
        if (args.Length == 0 || !int.TryParse(args[0], out n))
        {
            n = 27;
        }

        using var cts = new CancellationTokenSource();
        var ch = Channel.CreateBounded<int>(s_channelOptions);
        _ = GenerateAsync(ch.Writer, cts.Token);
        for (var i = 0; i < n; i++)
        {
            var prime = await ch.Reader.ReadAsync().ConfigureAwait(false);
            Console.WriteLine(prime);
            var chNext = Channel.CreateBounded<int>(s_channelOptions);
            _ = FilterAsync(ch.Reader, chNext.Writer, prime, cts.Token);
            ch = chNext;
        }
        cts.Cancel();
    }

    static async Task GenerateAsync(ChannelWriter<int> writer, CancellationToken ct)
    {
        for (var i = 2; !ct.IsCancellationRequested; i++)
        {
            await writer.WriteAsync(i, ct).ConfigureAwait(false);
        }
    }

    static async Task FilterAsync(
        ChannelReader<int> reader,
        ChannelWriter<int> writer,
        int prime,
        CancellationToken ct)
    {
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
