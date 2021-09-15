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

        public override string ToString()
        {
            return $"Cpu: {ModelName} (Model {Model})";
        }

        public static bool TryParse(string rawText, out CpuInfo cpuInfo)
        {
            Match match = Regex.Match(rawText, @"^Model:\s*(?<model>\d+)\s*$\s*^Model name:\s*(?<name>.+?)$", RegexOptions.Compiled | RegexOptions.Multiline | RegexOptions.IgnoreCase);
            if (match.Success)
            {
                cpuInfo = new CpuInfo
                {
                    ModelName = match.Groups["name"].Value.Trim(),
                    Model = int.Parse(match.Groups["model"].Value),
                    RawText = rawText,
                };
                return true;
            }
            cpuInfo = null;
            return false;
        }

        public static async Task<CpuInfo> LsCpuAsync()
        {
            StringBuilder stdout = new StringBuilder();
            await ProcessUtils.RunCommandAsync("lscpu", stdOutBuilder: stdout).ConfigureAwait(false);
            if (TryParse(stdout.ToString(), out CpuInfo cpuInfo))
            {
                return cpuInfo;
            }
            return null;
        }
    }
}
