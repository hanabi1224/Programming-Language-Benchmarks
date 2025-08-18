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

        using var output = Console.OpenStandardOutput();

        GenerateAsync(ch.Writer, cts.Token);

        for (var i = 0; i < n; i++)
        {
            if (!ch.Reader.TryRead(out var prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)

                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)

                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)

                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)

                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)

                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)

                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)

                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
                && !ch.Reader.TryRead(out prime)
            )
            {
                var readed = ch.Reader.ReadAsync();
                prime = readed.IsCompleted ? readed.Result : await readed.ConfigureAwait(false);
            }

            Span<byte> f = stackalloc byte[(int)Math.Ceiling(Math.Log10(prime)) + 1];
            var c = prime;
            var wI = 2;
            while (c != 0)
            {
                const byte zero = (byte)'0';
                f[^wI] = (byte)(zero + (c % 10));
                c = c / 10;
                wI += 1;
            }

            f[^1] = (byte)'\n';

            output.Write(f);

            var chNext = Channel.CreateBounded<int>(s_channelOptions);

            FilterAsync(ch.Reader, chNext.Writer, prime, cts.Token);

            ch = chNext;
        }
        cts.Cancel();
    }

    static void GenerateAsync(ChannelWriter<int> writer, CancellationToken ct)
    {
        ThreadPool.QueueUserWorkItem(async delegate
        {
            for (var i = 2; !ct.IsCancellationRequested; i++)
            {
                if (!writer.TryWrite(i))
                {
                    try
                    {
                        var writed = writer.WriteAsync(i, ct);
                        if (!writed.IsCompleted) await writed;
                    }
                    catch { }
                }
            }
        });
    }

    static void FilterAsync(
        ChannelReader<int> reader,
        ChannelWriter<int> writer,
        int prime,
        CancellationToken ct)
    {
        ThreadPool.QueueUserWorkItem(async delegate
        {
            while (!ct.IsCancellationRequested)
            {
                if (!reader.TryRead(out var n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)

                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)

                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)

                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)

                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)

                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)

                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                    && !reader.TryRead(out n)
                )
                {
                    try
                    {
                        var readed = reader.ReadAsync(ct);
                        n = readed.IsCompleted ? readed.Result : await readed.ConfigureAwait(false);
                    }
                    catch { }
                }

                if (n % prime != 0)
                {
                    if (!writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)

                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)

                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)

                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)

                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)

                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                        && !writer.TryWrite(n)
                    )
                    {
                        try
                        {
                            var writed = writer.WriteAsync(n, ct);
                            if (!writed.IsCompleted) await writed.ConfigureAwait(false);
                        }
                        catch { }
                    }
                }
            }
        });
    }
}
