using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

static class Program
{
    private static readonly HttpClient s_client = new HttpClient() { Timeout = TimeSpan.FromSeconds(1) };

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
        app.MapPost("/", async(HttpResponse response, [FromBody] Payload payload) =>
        {

            if (payload.Value.TryFormat(response.BodyWriter.GetSpan(16), out n))
            {
                response.StatusCode = StatusCodes.Status200OK;

                response.BodyWriter.Advance(n);
                await response.BodyWriter.FlushAsync().ConfigureAwait(false);
            }
            else
            {
                throw new UnreachableException();
            }


        });

        using var serverTask = app.RunAsync();
        var sum = 0;
        var api = $"http://localhost:{port}/";
        var tasks = new List<Task<int>>(n);
        for (var i = 1; i <= n; i++)
        {
            tasks.Add(SendAsync(api, i));
        }
        // await Task.WhenAll(tasks).ConfigureAwait(false);
        foreach (var task in tasks)
        {
            sum += await task.ConfigureAwait(false);
        }
        Console.WriteLine(sum);
        Environment.Exit(0);
    }

    private static async Task<int> SendAsync(string api, int value)
    {
        // await Task.Yield();
        var payload = new Payload { Value = value };
        while (true)
        {
            try
            {
                var response = await s_client.PostAsJsonAsync(api, payload).ConfigureAwait(false);
                return int.Parse(await response.Content.ReadAsByteArrayAsync().ConfigureAwait(false));
            }
            catch { }
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
            options.ListenLocalhost(port);
        });
        return builder.Build();
    }
}

public struct Payload
{
    [JsonPropertyName("value")]
    public int Value { get; set; }
}
