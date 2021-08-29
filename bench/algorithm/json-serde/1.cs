using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;

static class Program
{
    private static readonly JsonSerializerSettings s_Setting = new JsonSerializerSettings
    {
        Formatting = Formatting.Indented,
    };

    public static async Task Main(string[] args)
    {
        var fileName = args.Length > 0 ? args[0] : "sample";
        int n;
        if (args.Length < 2 || !int.TryParse(args[1], out n))
        {
            n = 3;
        }

        var jsonStr = await File.ReadAllTextAsync($"{fileName}.json").ConfigureAwait(false);
        for (var i = 1; i <= n; i++)
        {
            var data = JsonConvert.DeserializeObject(jsonStr);
            var sb = new StringBuilder();
            using var sw = new StringWriter(sb);
            using var writer = new JsonTextWriter(sw) { Indentation = i };
            var serializer = JsonSerializer.CreateDefault(s_Setting);
            serializer.Serialize(writer, data);
            using var hasher = MD5.Create();
            var hash = hasher.ComputeHash(Encoding.UTF8.GetBytes(sb.ToString()));
            Console.WriteLine(ToHexString(hash));
        }
    }

    static string ToHexString(byte[] ba)
    {
        StringBuilder hex = new StringBuilder(ba.Length * 2);
        foreach (byte b in ba)
            hex.AppendFormat("{0:x2}", b);
        return hex.ToString();
    }
}
