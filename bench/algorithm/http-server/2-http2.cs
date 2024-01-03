using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Server.Kestrel.Core;
using Microsoft.Extensions.Logging;

static class Program
{
    private static readonly HttpClient s_client = new HttpClient(new HttpClientHandler
    {
        ServerCertificateCustomValidationCallback = (_, _, _, _) => true,
    })
    {
        Timeout = TimeSpan.FromSeconds(1),
        DefaultRequestVersion = HttpVersion.Version20,
        DefaultVersionPolicy = HttpVersionPolicy.RequestVersionOrHigher,
    };

    static Program()
    {
        ServicePointManager.ReusePort = true;
        // https://docs.microsoft.com/en-US/troubleshoot/aspnet/performance-call-web-service
        ServicePointManager.DefaultConnectionLimit = 12 * Environment.ProcessorCount;
        // https://blogs.msdn.microsoft.com/windowsazurestorage/2010/06/25/nagles-algorithm-is-not-friendly-towards-small-requests/
        ServicePointManager.UseNagleAlgorithm = false;
    }

    public static async Task Main(string[] args)
    {
        int n;
        if (args.Length < 1 || !int.TryParse(args[0], out n))
        {
            n = 10;
        }

        var port = 30000 + new Random().Next(10000);
        var app = CreateWebApplication(port);
        app.MapPost("/", async ctx =>
        {
            using var sr = new StreamReader(ctx.Request.Body);
            var bodyText = await sr.ReadToEndAsync().ConfigureAwait(false);
            var payload = JsonSerializer.Deserialize<Payload>(bodyText);
            ctx.Response.StatusCode = 200;
            await ctx.Response.BodyWriter.WriteAsync(Encoding.UTF8.GetBytes(payload.Value.ToString())).ConfigureAwait(false);
        });

        using var serverTask = app.RunAsync();
        var sum = 0;
        var api = $"http://localhost:{port}/";
        var tasks = new List<Task<int>>(n);
        for (var i = 1; i <= n; i++)
        {
            tasks.Add(SendAsync(api, i));
        }
        foreach (var task in tasks)
        {
            sum += await task.ConfigureAwait(false);
        }
        Console.WriteLine(sum);
        Environment.Exit(0);
    }

    private static async Task<int> SendAsync(string api, int value)
    {
        var payload = JsonSerializer.Serialize(new Payload { Value = value });
        while (true)
        {
            try
            {
                var content = new StringContent(payload, Encoding.UTF8);
                var response = await s_client.PostAsync(api, content).ConfigureAwait(false);
                return int.Parse(await response.Content.ReadAsStringAsync().ConfigureAwait(false));
            }
            catch (Exception e)
            {
#if DEBUG
                Console.Error.WriteLine(e);
#endif
            }
        }
    }

    private static WebApplication CreateWebApplication(int port)
    {
        var builder = WebApplication.CreateBuilder();
        builder.WebHost.ConfigureLogging((context, logging) =>
        {
            logging.ClearProviders();
        }).UseKestrel(options =>
        {
            options.Limits.MaxRequestBodySize = null;
            options.ListenLocalhost(port, listenOptions =>
            {
                listenOptions.Protocols = HttpProtocols.Http2;
            });
        });
        return builder.Build();
    }
}

public struct Payload
{
    [JsonPropertyName("value")]
    public int Value { get; set; }
}
