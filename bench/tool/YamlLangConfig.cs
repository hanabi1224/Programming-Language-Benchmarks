using System.Collections.Generic;

namespace BenchTool
{
    public class YamlBenchmarkConfig
    {
        public bool Udocker { get; set; }

        public List<YamlBenchmarkProblemConfig> Problems { get; set; }

        public List<YamlLangConfig> Langs { get; set; }
    }

    public class YamlBenchmarkProblemConfig
    {
        public string Name { get; set; }

        public string[] Data { get; set; }

        public string[] DataSetupCmd { get; set; }

        public YamlBenchmarkProblemUnittestConfig[] Unittests { get; set; }

        public YamlBenchmarkProblemTestConfig[] Tests { get; set; }
    }

    public class YamlBenchmarkProblemUnittestConfig
    {
        public string Input { get; set; }

        public string Output { get; set; }
    }

    public class YamlBenchmarkProblemTestConfig
    {
        public string Input { get; set; }

        public int Repeat { get; set; } = 3;

        public int TimeoutSeconds { get; set; } = 5;

        public bool SkipOnPullRequest { get; set; } = false;

        public HashSet<string> ExcludeLangs { get; set; }
    }

    public abstract class LangConfigBase
    {
        public string CompilerVersionCommand { get; set; }

        public string CompilerVersionRegex { get; set; }

        public string RuntimeVersionParameter { get; set; }

        public string RuntimeVersionRegex { get; set; }

        public string SourceRenameTo { get; set; }

        public bool Enabled { get; set; } = true;
    }

    public class YamlLangConfig : LangConfigBase
    {
        public string Lang { get; set; }

        public YamlLangProblemConfig[] Problems { get; set; }

        public YamlLangEnvironmentConfig[] Environments { get; set; }
    }

    public class YamlLangProblemConfig
    {
        public string Name { get; set; }

        public string[] Source { get; set; }
    }

    public class YamlLangEnvironmentConfig : LangConfigBase
    {
        public string Os { get; set; }

        public string Compiler { get; set; }

        public string Version { get; set; }

        public string CompilerOptions { get; set; }

        public string CompilerOptionsText { get; set; } = "default";

        public string Docker { get; set; }

        public string DockerRuntimeDir { get; set; }

        public string DockerRuntimeFile { get; set; }

        public string[] DockerVolumns { get; set; }

        public Dictionary<string, string> Env { get; set; }

        public string Include { get; set; }

        public string IncludeSubDir { get; set; }

        public string[] BeforeBuild { get; set; }

        public string Build { get; set; }

        public string[] AfterBuild { get; set; }

        public string OutDir { get; set; } = "out";

        public string[] BeforeRun { get; set; }

        public string RunCmd { get; set; }

        public Dictionary<string, string> RunCmdEnv { get; set; }

        public bool RuntimeIncluded { get; set; } = true;

        public bool ForceCheckChildProcesses { get; set; } = false;

        public bool AllowFailure { get; set; } = false;
    }
}
