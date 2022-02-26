using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Reflection;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
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
        var server = CreateWebHostBuilder(port).Build();
        using var cts = new CancellationTokenSource();
        _ = Task.Factory.StartNew(() => server.Start(), TaskCreationOptions.LongRunning);
        // _ = server.RunAsync(cts.Token);
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
        System.Environment.Exit(0);
    }

    private static async Task<int> SendAsync(string api, int value)
    {
        // await Task.Yield();
        var payload = JsonSerializer.Serialize(new Payload { Value = value });
        while (true)
        {
            try
            {
                var content = new StringContent(payload, Encoding.UTF8);
                var response = await s_client.PostAsync(api, content).ConfigureAwait(false);
                return int.Parse(await response.Content.ReadAsStringAsync().ConfigureAwait(false));
            }
            catch { }
        }
    }

    private static IWebHostBuilder CreateWebHostBuilder(int port)
    {
        return WebHost.CreateDefaultBuilder()
            .SuppressStatusMessages(true)
            .ConfigureLogging((context, logging) =>
            {
                logging.ClearProviders();
            })
            .UseKestrel(options =>
            {
                options.Limits.MaxRequestBodySize = null;
                options.ListenLocalhost(port);
            })
            .UseStartup<Startup>();
    }
}

public sealed class MyController : Controller
{
    [Route("/")]
    public async Task<int> PostAsync()
    {
        using var sr = new StreamReader(Request.Body);
        var bodyText = await sr.ReadToEndAsync().ConfigureAwait(false);
        var payload = JsonSerializer.Deserialize<Payload>(bodyText);
        return payload.Value;
    }
}

public struct Payload
{
    [JsonPropertyName("value")]
    public int Value { get; set; }
}

public sealed class Startup
{
    public Startup(IConfiguration configuration)
    {
        Configuration = configuration;
    }

    public IConfiguration Configuration { get; }

    public void ConfigureServices(IServiceCollection services)
    {
        services.AddMvcCore().AddApplicationPart(Assembly.GetExecutingAssembly());
    }

    public void Configure(
        IApplicationBuilder app,
        IWebHostEnvironment env)
    {
        app.UseRouting();
        app.UseEndpoints(endpoints =>
        {
            endpoints.MapControllers();
        });
    }
}
