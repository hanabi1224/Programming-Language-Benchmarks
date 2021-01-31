using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace BenchTool
{
    public class BuildOutputJson
    {
        public const string FileName = "__build_output.json";

        public BuildOutputJson()
        {
            Start = DateTimeOffset.UtcNow;
        }

        [JsonProperty("compilerVersion")]
        public string CompilerVersionText { get; set; }

        [JsonProperty("start")]
        public DateTimeOffset Start { get; set; }

        [JsonProperty("finished")]
        public DateTimeOffset Finished { get; set; }

        [JsonProperty("durationMs")]
        public long DurationMs { get; set; }

        public static string GetFilePath(string dir)
        {
            return Path.Combine(dir, FileName);
        }

        public static BuildOutputJson LoadFrom(string dir)
        {
            var path = GetFilePath(dir);
            if (!File.Exists(path))
            {
                return new BuildOutputJson();
            }

            var content = File.ReadAllText(path);
            return JsonConvert.DeserializeObject<BuildOutputJson>(content);
        }

        public Task SaveAsync(string dir)
        {
            Finished = DateTimeOffset.UtcNow;
            DurationMs = (long)(Finished - Start).TotalMilliseconds;
            var path = GetFilePath(dir);
            var contents = JsonConvert.SerializeObject(this, Formatting.Indented);
            return File.WriteAllTextAsync(path: path, contents: contents);
        }
    }

    public class TestOutputJson
    {
        public const string FileName = "__test_output.json";

        public TestOutputJson()
        {
            Start = DateTimeOffset.UtcNow;
        }

        [JsonProperty("runtimeVersion")]
        public string RuntimeVersionText { get; set; }

        [JsonProperty("start")]
        public DateTimeOffset Start { get; set; }

        [JsonProperty("finished")]
        public DateTimeOffset Finished { get; set; }

        [JsonProperty("durationMs")]
        public long DurationMs { get; set; }

        public static string GetFilePath(string dir)
        {
            return Path.Combine(dir, FileName);
        }

        public static TestOutputJson LoadFrom(string dir)
        {
            var path = GetFilePath(dir);
            if (!File.Exists(path))
            {
                return new TestOutputJson();
            }

            var content = File.ReadAllText(path);
            return JsonConvert.DeserializeObject<TestOutputJson>(content);
        }

        public Task SaveAsync(string dir)
        {
            Finished = DateTimeOffset.UtcNow;
            DurationMs = (long)(Finished - Start).TotalMilliseconds;
            var path = GetFilePath(dir);
            var contents = JsonConvert.SerializeObject(this, Formatting.Indented);
            return File.WriteAllTextAsync(path: path, contents: contents);
        }
    }
}
