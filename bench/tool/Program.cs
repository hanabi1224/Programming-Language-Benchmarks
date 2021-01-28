using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Win32.SafeHandles;
using Newtonsoft.Json;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace BenchTool
{
    public static class Program
    {
        const string TaskBuild = "build";
        const string TaskTest = "test";
        const string TaskBench = "bench";

        /// <summary>
        /// Main function
        /// </summary>
        /// <param name="config">Path to benchmark config file</param>
        /// <param name="algorithm">Root path that contains all algorithm code</param>
        /// <param name="include">Root path that contains all include project templates</param>
        /// <param name="buildOutput">Output folder of build step</param>
        /// <param name="task">Benchmark task to run, valid values: build, test, bench</param>
        /// <param name="forcePullDocker">A flag that indicates whether to force pull docker image even when it exists</param>
        /// <param name="failFast">A Flag that indicates whether to fail fast when error occurs</param>
        /// <param name="langs">Languages to incldue, e.g. --langs go csharp</param>
        /// <param name="environments">OS environments to incldue, e.g. --environments linux windows</param>
        public static async Task Main(
            string config = "bench.yaml",
            string algorithm = "algorithm",
            string include = "include",
            string buildOutput = "build",
            string task = "build",
            bool forcePullDocker = false,
            bool failFast = false,
            string[] langs = null,
            string[] environments = null)
        {
            config.EnsureFileExists();
            algorithm.EnsureDirectoryExists();
            include.EnsureDirectoryExists();

            buildOutput.CreateDirectoryIfNotExist();

            if (!new HashSet<string> { TaskBuild, TaskTest, TaskBench }.Contains(task))
            {
                throw new NotSupportedException($"Unknown task: {task}");
            }

            var yamlDeserializer = new DeserializerBuilder()
                .WithNamingConvention(UnderscoredNamingConvention.Instance)
                .Build();

            var yamlStr = File.ReadAllText(config);
            var benchConfig = yamlDeserializer.Deserialize<YamlBenchmarkConfig>(yamlStr);
            var langConfigs = benchConfig.Langs;
            var includedLanguages = new HashSet<string>(langs ?? new string[] { }, StringComparer.OrdinalIgnoreCase);
            var includedOsEnvironments = new HashSet<string>(environments ?? new string[] { }, StringComparer.OrdinalIgnoreCase);
            foreach (var c in langConfigs)
            {
                if (includedLanguages.Count > 0
                    && !includedLanguages.Contains(c.Lang))
                {
                    continue;
                }

                foreach (var env in c.Environments)
                {
                    if (includedOsEnvironments.Count > 0
                        && !includedOsEnvironments.Contains(env.Os))
                    {
                        continue;
                    }

                    foreach (var p in c.Problems)
                    {
                        foreach (var codePath in p.Source)
                        {
                            try
                            {
                                var buildId = $"{c.Lang}_{env.Os}_{env.Compiler}_{env.Version}_{env.CompilerOptionsText}_{p.Name}_{Path.GetFileNameWithoutExtension(codePath)}";
                                Console.WriteLine($"{task}: {buildId}");

                                switch (task)
                                {
                                    case TaskBuild:
                                        await BuildAsync(buildId, c, env, p, codePath: codePath, algorithmDir: algorithm, buildOutputDir: buildOutput, includeDir: include, forcePullDocker: forcePullDocker).ConfigureAwait(false);
                                        break;
                                    case TaskTest:
                                        await TestAsync(buildId, benchConfig, c, env, p, algorithmDir: algorithm, buildOutputDir: buildOutput).ConfigureAwait(failFast);
                                        break;
                                    case TaskBench:
                                        await BenchAsync(buildId, benchConfig, c, env, p, codePath: codePath, algorithmDir: algorithm, buildOutputDir: buildOutput).ConfigureAwait(failFast);
                                        break;
                                }
                            }
                            catch (Exception e)
                            {
                                if (failFast)
                                {
                                    throw;
                                }
                                else
                                {
                                    Console.Error.WriteLine(e);
                                }
                            }
                        }
                    }
                }
            }
        }

        private static async Task BuildAsync(
            string buildId,
            YamlLangConfig langConfig,
            YamlLangEnvironmentConfig langEnvConfig,
            YamlLangProblemConfig problem,
            string codePath,
            string algorithmDir,
            string buildOutputDir,
            string includeDir,
            bool forcePullDocker)
        {
            var buildOutput = Path.Combine(Environment.CurrentDirectory, buildOutputDir, buildId);
            if (Directory.Exists(buildOutput))
            {
                Console.WriteLine($"Build cache hit.");
                return;
            }

            // Source code
            var srcCodePath = Path.Combine(algorithmDir, problem.Name, codePath);
            srcCodePath.EnsureFileExists();

            // Setup tmp build folder
            using var tmpDir = new TempFolder();

            Console.WriteLine($"Temp build folder: {tmpDir.FullPath}");

            // Copy Include folder
            if (!langEnvConfig.Include.IsEmptyOrWhiteSpace())
            {
                var fromDir = Path.Combine(includeDir, langEnvConfig.Include);
                fromDir.EnsureDirectoryExists();

                await ProcessUtils.RunCommandAsync($"cp -a \"{fromDir}\"  \"{tmpDir.FullPath}\"").ConfigureAwait(false);
            }

            var srcCodeDestDir = langEnvConfig.IncludeSubDir.IsEmptyOrWhiteSpace() ? tmpDir.FullPath : Path.Combine(tmpDir.FullPath, langEnvConfig.IncludeSubDir);
            srcCodeDestDir.CreateDirectoryIfNotExist();
            var srcCodeDestFileName = langEnvConfig.SourceRenameTo
                .FallBackTo(langConfig.SourceRenameTo)
                .FallBackTo(Path.GetFileName(srcCodePath));
            File.Copy(srcCodePath, Path.Combine(srcCodeDestDir, srcCodeDestFileName));

            // Docker setup
            var docker = langEnvConfig.Docker;
            var useDocker = !docker.IsEmptyOrWhiteSpace();
            if (useDocker && forcePullDocker)
            {
                await ProcessUtils.RunCommandAsync($"docker pull {docker}").ConfigureAwait(false);
            }

            // Before Build
            var beforeBuild = langEnvConfig.BeforeBuild;
            if (!beforeBuild.IsEmptyOrWhiteSpace())
            {
                await ProcessUtils.RunCommandAsync(beforeBuild, workingDir: tmpDir.FullPath).ConfigureAwait(false);
            }

            // Check compiler version and save output
            var compilerVersionCommand = langEnvConfig.CompilerVersionCommand.FallBackTo(langConfig.CompilerVersionCommand);
            if (useDocker)
            {
                compilerVersionCommand = $"docker run --rm {docker} {compilerVersionCommand}";
            }

            await ProcessUtils.RunCommandAsync(compilerVersionCommand, workingDir: tmpDir.FullPath).ConfigureAwait(false);

            // Build
            var buildCommand = langEnvConfig.Build;
            if (useDocker)
            {
                const string DockerTmpCodeDir = "/tmp/code";
                buildCommand = $"docker run --rm -v {tmpDir.FullPath}:{DockerTmpCodeDir} -w {DockerTmpCodeDir} {docker} sh -c \"{buildCommand}\"";
            }

            await ProcessUtils.RunCommandAsync(buildCommand, workingDir: tmpDir.FullPath).ConfigureAwait(false);

            if (Directory.Exists(buildOutput))
            {
                Directory.Delete(buildOutput, recursive: true);
            }

            var tmpBuildOutput = $"{Path.Combine(tmpDir.FullPath, langEnvConfig.OutDir)}";
            Console.WriteLine($"Copying from {tmpBuildOutput} to {buildOutput}");
            Directory.Move(tmpBuildOutput, buildOutput);
            Console.WriteLine($"Copied from {tmpBuildOutput} to {buildOutput}");
            await Task.Delay(TimeSpan.FromSeconds(1)).ConfigureAwait(false);
            //await ProcessUtils.RunAsync($"cp -a \"{Path.Combine(tmpDir.FullPath, langEnvConfig.OutDir)}\" \"{buildOutput}\"").ConfigureAwait(false);
        }

        private static async Task TestAsync(
            string buildId,
            YamlBenchmarkConfig benchConfig,
            YamlLangConfig langConfig,
            YamlLangEnvironmentConfig langEnvConfig,
            YamlLangProblemConfig problem,
            string algorithmDir,
            string buildOutputDir)
        {
            var buildOutput = Path.Combine(Environment.CurrentDirectory, buildOutputDir, buildId);
            buildOutput.EnsureDirectoryExists();

            var exeName = Path.Combine(buildOutput, langEnvConfig.RunCmd.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0]);
            await ProcessUtils.RunCommandAsync($"chmod +x \"{exeName}\"", workingDir: buildOutput).ConfigureAwait(false);

            var problemTestConfig = benchConfig.Problems.FirstOrDefault(i => i.Name == problem.Name);
            foreach (var test in problemTestConfig.Unittests)
            {
                var expectedOutputPath = Path.Combine(algorithmDir, problem.Name, test.Output);
                expectedOutputPath.EnsureFileExists();

                var expectedOutput = File.ReadAllText(expectedOutputPath);

                var runCommand = $"{langEnvConfig.RunCmd} {test.Input}";

                var runPsi = runCommand.ConvertToCommand();
                runPsi.FileName = exeName;
                runPsi.WorkingDirectory = buildOutput;
                ProcessUtils.RunProcess(runPsi, printOnConsole: false, out var stdOut, out var stdErr, default);
                if (StringComparer.Ordinal.Equals(expectedOutput.TrimEnd(), stdOut.TrimEnd()))
                {
                    Console.WriteLine($"Test Passed: {buildId}");
                }
                else
                {
                    Console.Error.WriteLine($"Test Failed: {buildId}");
                    Console.WriteLine(stdOut);
                    Console.Error.WriteLine(stdErr);
                }

                Console.WriteLine();
            }
        }

        private static async Task BenchAsync(
                string buildId,
                YamlBenchmarkConfig benchConfig,
                YamlLangConfig langConfig,
                YamlLangEnvironmentConfig langEnvConfig,
                YamlLangProblemConfig problem,
                string codePath,
                string algorithmDir,
                string buildOutputDir)
        {
            var buildOutput = Path.Combine(Environment.CurrentDirectory, buildOutputDir, buildId);
            buildOutput.EnsureDirectoryExists();

            var benchResultDir = Path.Combine(Environment.CurrentDirectory, buildOutputDir, "_results", langConfig.Lang);
            benchResultDir.CreateDirectoryIfNotExist();
            var benchResultJsonPath = Path.Combine(benchResultDir, $"{buildId}.json");

            var exeName = Path.Combine(buildOutput, langEnvConfig.RunCmd.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0]);
            var problemTestConfig = benchConfig.Problems.FirstOrDefault(i => i.Name == problem.Name);
            foreach (var test in problemTestConfig.Tests)
            {
                var runCommand = $"{langEnvConfig.RunCmd} {test.Input}";

                var runPsi = runCommand.ConvertToCommand();
                runPsi.FileName = exeName;
                runPsi.WorkingDirectory = buildOutput;

                var repeat = test.Repeat > 1 ? test.Repeat : 1;
                var measurements = new List<ProcessMeasurement>(repeat);
                for (var i = 0; i < repeat; i++)
                {
                    var measurement = await ProcessUtils.MeasureAsync(runPsi).ConfigureAwait(false);
                    Console.WriteLine($"{buildId} {measurement}");
                    measurements.Add(measurement);
                }

                var avgMeasurement = measurements.GetAverage();
                Console.ForegroundColor = ConsoleColor.Cyan;
                Console.WriteLine($"\n\n[AVG] {buildId} {avgMeasurement}\n\n");
                Console.ForegroundColor = ConsoleColor.White;

                await File.WriteAllTextAsync(benchResultJsonPath, JsonConvert.SerializeObject(new
                {
                    lang = langConfig.Lang,
                    os = langEnvConfig.Os,
                    compiler = langEnvConfig.Compiler,
                    compilerVersion = langEnvConfig.Version,
                    test = problem.Name,
                    code = codePath,
                    input = test.Input,
                    timeMS = avgMeasurement.Elapsed.TotalMilliseconds,
                    memBytes = avgMeasurement.PeakMemoryBytes,
                    cpuTimeMS = avgMeasurement.CpuTime.TotalMilliseconds,
                }, Formatting.Indented)).ConfigureAwait(false);
            }
        }
    }
}
