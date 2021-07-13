using System;
using System.IO;
using System.Net.Http;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;

static class Program
{
    private const string MimeType = "application/json";
    private static readonly HttpClient s_client = new HttpClient();

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
        _ = server.RunAsync(cts.Token);
        var sum = 0;
        var api = $"http://localhost:{port}/";
        for (var i = 1; i <= n; i++)
        {
            sum += await SendAsync(api, i);
        }
        Console.WriteLine(sum);
    }

    private static async Task<int> SendAsync(string api, int value)
    {
        var payload = JsonConvert.SerializeObject(new Payload { Value = value });
        while (true)
        {
            try
            {
                var content = new StringContent(payload, Encoding.UTF8, MimeType);
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
            //.UseUrls($"http://localhost:{port}/")
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
        var payload = JsonConvert.DeserializeObject<Payload>(bodyText);
        return payload.Value;
    }
}

public class Payload
{
    [JsonProperty("value")]
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
