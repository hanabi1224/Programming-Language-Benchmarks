using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization.Metadata;
using System.Threading.Tasks;

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
        var jti = JsonTypeInfo.CreateJsonTypeInfo<Object>(JsonSerializerOptions.Default);
        var data = JsonSerializer.Deserialize(jsonStr, jti);
        
        PrintHash(JsonSerializer.Serialize(data));
        var list = new List<object>(n);
        for (var i = 0; i < n; i++)
        {
            list.Add(JsonSerializer.Deserialize(jsonStr, jti));
        }
        PrintHash(JsonSerializer.Serialize(list));
    }

    private static void PrintHash(string s)
    {
        using var hasher = MD5.Create();
        var hash = hasher.ComputeHash(Encoding.UTF8.GetBytes(s));
        Console.WriteLine(ToHexString(hash));
    }

    static string ToHexString(byte[] ba)
    {
        StringBuilder hex = new StringBuilder(ba.Length * 2);
        foreach (byte b in ba)
            hex.AppendFormat("{0:x2}", b);
        return hex.ToString();
    }
}
