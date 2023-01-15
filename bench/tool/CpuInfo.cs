using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace BenchTool
{
    public class CpuInfo
    {
        public string ModelName { get; private set; }

        public int Model { get; private set; }

        public string RawText { get; private set; }

        public int NumOfCores { get; private set; }

        public string Architecture { get; private set; }

        public override string ToString()
        {
            return $"[{Architecture}][{NumOfCores} cores] {ModelName} (Model {Model})";
        }

        public static bool TryParse(string rawText, out CpuInfo cpuInfo)
        {
            Match match = Regex.Match(rawText, @"^Architecture:\s*(?<arch>.+?)\s*$[^.$]*?^CPU\(s\):\s*(?<core>\d+)\s*$[^.$]*?^Model name:\s*(?<name>.+?)$[^.$]*?^Model:\s*(?<model>\d+)\s*$", RegexOptions.Compiled | RegexOptions.Multiline | RegexOptions.IgnoreCase);
            if (match.Success)
            {
                cpuInfo = new CpuInfo
                {
                    ModelName = match.Groups["name"].Value.Trim(),
                    Model = int.Parse(match.Groups["model"].Value),
                    RawText = rawText,
                    NumOfCores = int.Parse(match.Groups["core"].Value),
                    Architecture = match.Groups["arch"].Value.Trim(),
                };
                return true;
            }
            cpuInfo = null;
            return false;
        }

        public static async Task<CpuInfo> LsCpuAsync()
        {
            StringBuilder stdout = new StringBuilder();
            try
            {
                await ProcessUtils.RunCommandAsync("lscpu", stdOutBuilder: stdout).ConfigureAwait(false);
                if (TryParse(stdout.ToString(), out CpuInfo cpuInfo))
                {
                    return cpuInfo;
                }
            }
            catch { }
            return null;
        }
    }
}
