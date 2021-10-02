using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;

internal static class Program
{
    public static async Task Main(string[] args)
    {
        string fileName = args.Length > 0 ? args[0] : "sample";
        if (args.Length < 2 || !int.TryParse(args[1], out int n))
        {
            n = 10;
        }

        string jsonStr = await File.ReadAllTextAsync($"{fileName}.json").ConfigureAwait(false);
        GeoData data = JsonSerializer.Deserialize<GeoData>(jsonStr);
        PrintHash(JsonSerializer.SerializeToUtf8Bytes(data));
        List<GeoData> array = new List<GeoData>(n);
        for (int i = 0; i < n; i++)
        {
            array.Add(JsonSerializer.Deserialize<GeoData>(jsonStr));
        }
        PrintHash(JsonSerializer.SerializeToUtf8Bytes(array));
    }

    private static void PrintHash(byte[] bytes)
    {
        using MD5 hasher = MD5.Create();
        byte[] hash = hasher.ComputeHash(bytes);
        Console.WriteLine(ToHexString(hash));
    }

    private static string ToHexString(byte[] ba)
    {
        StringBuilder hex = new StringBuilder(ba.Length * 2);
        foreach (byte b in ba)
        {
            hex.AppendFormat("{0:x2}", b);
        }
        return hex.ToString();
    }
}

internal class GeoData
{
    [JsonPropertyName("type")]
    public string Type { get; set; }

    [JsonPropertyName("features")]
    public Feature[] Features { get; set; }
}

internal class Feature
{
    [JsonPropertyName("type")]
    public string Type { get; set; }

    [JsonPropertyName("properties")]
    public Properties Properties { get; set; }

    [JsonPropertyName("geometry")]
    public Geometry Geometry { get; set; }
}

internal class Properties
{
    [JsonPropertyName("name")]
    public string Name { get; set; }
}

internal class Geometry
{
    [JsonPropertyName("type")]
    public string Type { get; set; }

    [JsonPropertyName("coordinates")]
    public double[][][] Coordinates { get; set; }
}