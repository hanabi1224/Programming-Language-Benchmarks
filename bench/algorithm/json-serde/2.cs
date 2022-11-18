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
        string jsonStr = (await File.ReadAllTextAsync($"{fileName}.json").ConfigureAwait(false))!;
        GeoData data = (JsonSerializer.Deserialize(jsonStr, MyJsonContext.Default.GeoData))!;
        PrintHash(JsonSerializer.SerializeToUtf8Bytes(data, MyJsonContext.Default.GeoData));
        GeoData[] array= new GeoData[n];
        for (int i = 0; i < n; i++)
        {
            array[i] = JsonSerializer.Deserialize(jsonStr, MyJsonContext.Default.GeoData);
        }
        PrintHash(JsonSerializer.SerializeToUtf8Bytes(array));
    }
    private static void PrintHash(byte[] bytes)
    {
        using MD5 hasher = MD5.Create();
        byte[] hash = hasher.ComputeHash(bytes);
        Console.WriteLine(Convert.ToHexString(hash).ToLower());
    }
}
internal sealed class GeoData
{
    [JsonPropertyName("type")]
    public string Type { get; set; }

    [JsonPropertyName("features")]
    public Feature[] Features { get; set; }
}

internal sealed class Feature
{
    [JsonPropertyName("type")]
    public string Type { get; set; }

    [JsonPropertyName("properties")]
    public Properties Properties { get; set; }

    [JsonPropertyName("geometry")]
    public Geometry Geometry { get; set; }
}

internal sealed class Properties
{
    [JsonPropertyName("name")]
    public string Name { get; set; }
}

internal sealed class Geometry
{
    [JsonPropertyName("type")]
    public string Type { get; set; }

    [JsonPropertyName("coordinates")]
    public double[][][] Coordinates { get; set; }
}

[JsonSerializable(typeof(GeoData))]
internal sealed partial class MyJsonContext : JsonSerializerContext { }
