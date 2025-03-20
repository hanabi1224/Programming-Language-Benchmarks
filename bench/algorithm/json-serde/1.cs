using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;

static class Program
{
    public static async Task Main(string[] args)
    {
        var fileName = args.Length > 0 ? args[0] : "sample";
        int n;
        if (args.Length < 2 || !int.TryParse(args[1], out n))
        {
            n = 10;
        }

        var jsonStr = await File.ReadAllTextAsync($"{fileName}.json").ConfigureAwait(false);
        var data = JsonConvert.DeserializeObject(jsonStr);
        PrintHash(JsonConvert.SerializeObject(data));
        var list = new List<object>(n);
        for (var i = 0; i < n; i++)
        {
            list.Add(JsonConvert.DeserializeObject(jsonStr));
        }
        PrintHash(JsonConvert.SerializeObject(list));
    }

    private static void PrintHash(string s)
    {
        using var hasher = MD5.Create();
        var hash = hasher.ComputeHash(Encoding.UTF8.GetBytes(s));
        Console.WriteLine(Convert.ToHexStringLower(hash));
    }
}
