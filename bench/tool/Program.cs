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
using NLog;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace BenchTool
{
    public static class Program
    {
        private static Logger Logger { get; } = LogManager.GetCurrentClassLogger();

        const string TimerPrefix = "[Timer] ";

        static Program()
        {
            NLogUtils.Configure();
        }

        const string TaskBuild = "build";
        const string TaskTest = "test";
        const string TaskBench = "bench";

        private static bool _verbose = false;
        /// <summary>
        /// Main function
        /// </summary>
        /// <param name="config">Path to benchmark config file</param>
        /// <param name="algorithm">Root path that contains all algorithm code</param>
        /// <param name="include">Root path that contains all include project templates</param>
        /// <param name="buildOutput">Output folder of build step</param>
        /// <param name="task">Benchmark task to run, valid values: build, test, bench</param>
        /// <param name="forcePullDocker">A flag that indicates whether to force pull docker image even when it exists</param>
        /// <param name="forceRebuild">A flag that indicates whether to force rebuild</param>
        /// <param name="failFast">A Flag that indicates whether to fail fast when error occurs</param>
        /// <param name="verbose">A Flag that indicates whether to print verbose infomation</param>
        /// <param name="langs">Languages to incldue, e.g. --langs go csharp</param>
        /// <param name="problems">Problems to incldue, e.g. --problems binarytrees nbody</param>
        /// <param name="environments">OS environments to incldue, e.g. --environments linux windows</param>
        public static async Task Main(
            string config = "bench.yaml",
            string algorithm = "algorithm",
            string include = "include",
            string buildOutput = "build",
            string task = "build",
            bool forcePullDocker = false,
            bool forceRebuild = false,
            bool failFast = false,
            bool verbose = false,
            string[] langs = null,
            string[] problems = null,
            string[] environments = null)
        {
            var timer = Stopwatch.StartNew();
            _verbose = verbose;
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
            var configDir = Path.GetDirectoryName(config);
            foreach (var lcPath in Directory.GetFiles(configDir.FallBackTo("."), "bench_*.yaml", SearchOption.TopDirectoryOnly))
            {
                Logger.Debug($"Loading {lcPath}");
                var lc = yamlDeserializer.Deserialize<YamlLangConfig>(File.ReadAllText(lcPath));
                benchConfig.Langs.Add(lc);
            }

            var langConfigs = benchConfig.Langs;
            var includedLanguages = new HashSet<string>(langs ?? new string[] { }, StringComparer.OrdinalIgnoreCase);
            var includedOsEnvironments = new HashSet<string>(environments ?? new string[] { }, StringComparer.OrdinalIgnoreCase);
            var includedProblems = new HashSet<string>(problems ?? new string[] { }, StringComparer.OrdinalIgnoreCase);

            var aggregatedExceptions = new List<Exception>();
            foreach (var c in langConfigs.OrderBy(i => i.Lang))
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
                        if (includedProblems.Count > 0
                            && !includedProblems.Contains(p.Name))
                        {
                            continue;
                        }

                        foreach (var codePath in p.Source)
                        {
                            var taskTimer = Stopwatch.StartNew();
                            try
                            {
                                var buildId = $"{c.Lang}_{env.Os}_{env.Compiler}_{env.Version}_{env.CompilerOptionsText}_{p.Name}_{Path.GetFileNameWithoutExtension(codePath)}";
                                Logger.Info($"{task}: {buildId}");

                                switch (task)
                                {
                                    case TaskBuild:
                                        await BuildAsync(buildId, c, env, p, codePath: codePath, algorithmDir: algorithm, buildOutputDir: buildOutput, includeDir: include, forcePullDocker: forcePullDocker, forceRebuild: forceRebuild).ConfigureAwait(false);
                                        break;
                                    case TaskTest:
                                        await TestAsync(buildId, benchConfig, c, env, p, algorithmDir: algorithm, buildOutputDir: buildOutput).ConfigureAwait(failFast);
                                        break;
                                    case TaskBench:
                                        await BenchAsync(buildId, benchConfig, c, env, p, codePath: codePath, algorithmDir: algorithm, buildOutputDir: buildOutput).ConfigureAwait(failFast);
                                        break;
                                }

                                taskTimer.Stop();
                                Logger.Info($"{TimerPrefix}Job ({task}){buildId} finished in {taskTimer.Elapsed}");
                            }
                            catch (Exception e)
                            {
                                if (failFast)
                                {
                                    throw;
                                }
                                else
                                {
                                    aggregatedExceptions.Add(e);
                                    Logger.Error(e);
                                }
                            }
                        }
                    }
                }
            }

            if (aggregatedExceptions?.Count > 0)
            {
                throw new AggregateException(aggregatedExceptions);
            }

            timer.Stop();
            Logger.Info($"{TimerPrefix}Task {task} finished in {timer.Elapsed}");
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
            bool forcePullDocker,
            bool forceRebuild)
        {
            var buildOutput = Path.Combine(Environment.CurrentDirectory, buildOutputDir, buildId);
            if (!forceRebuild
                && buildOutput.IsDirectoryNotEmpty())
            {
                Logger.Debug($"Build cache hit.");
                return;
            }

            // Source code
            var srcCodePath = Path.Combine(algorithmDir, problem.Name, codePath);
            srcCodePath.EnsureFileExists();

            // Setup tmp build folder
            using var tmpDir = new TempFolder();

            Logger.Debug($"Temp build folder: {tmpDir.FullPath}");

            // Copy Include folder
            if (!langEnvConfig.Include.IsEmptyOrWhiteSpace())
            {
                var fromDir = Path.Combine(includeDir, langEnvConfig.Include);
                fromDir.EnsureDirectoryExists();

                await ProcessUtils.RunCommandAsync($"cp -a \"{fromDir}\"  \"{tmpDir.FullPath}\"", asyncRead: false).ConfigureAwait(false);
            }

            var tmpBuildOutput = Path.Combine(tmpDir.FullPath, langEnvConfig.OutDir ?? string.Empty);
            tmpBuildOutput.CreateDirectoryIfNotExist();

            var srcCodeDestDir = langEnvConfig.IncludeSubDir.IsEmptyOrWhiteSpace() ? tmpDir.FullPath : Path.Combine(tmpDir.FullPath, langEnvConfig.IncludeSubDir);
            srcCodeDestDir.CreateDirectoryIfNotExist();

            var srcCodeDestFileName = langEnvConfig.SourceRenameTo
                .FallBackTo(langConfig.SourceRenameTo)
                .FallBackTo(Path.GetFileName(srcCodePath));
            var srcCodeDestPath = Path.Combine(srcCodeDestDir, srcCodeDestFileName);
            Logger.Debug($"Copying {srcCodePath} to {srcCodeDestPath}");
            File.Copy(srcCodePath, srcCodeDestPath, overwrite: true);
            if (_verbose)
            {
                await ProcessUtils.RunCommandAsync($"ls -al \"{tmpDir.FullPath}\"", asyncRead: false).ConfigureAwait(false);
            }

            // Docker setup
            var docker = langEnvConfig.Docker;
            var useDocker = !docker.IsEmptyOrWhiteSpace();
            if (useDocker && forcePullDocker)
            {
                await ProcessUtils.RunCommandAsync($"docker pull {docker}").ConfigureAwait(false);
            }

            // Before Build
            await ProcessUtils.RunCommandsAsync(
                langEnvConfig.BeforeBuild,
                workingDir: tmpDir.FullPath).ConfigureAwait(false);

            // Check compiler version and save output
            var compilerVersionCommand = langEnvConfig.CompilerVersionCommand.FallBackTo(langConfig.CompilerVersionCommand);
            if (!compilerVersionCommand.IsEmptyOrWhiteSpace())
            {
                if (useDocker)
                {
                    compilerVersionCommand = $"docker run --rm {docker} {compilerVersionCommand}";
                }

                await ProcessUtils.RunCommandAsync(
                    compilerVersionCommand,
                    workingDir: tmpDir.FullPath).ConfigureAwait(false);
            }

            // Build
            var buildCommand = langEnvConfig.Build;
            if (!buildCommand.IsEmptyOrWhiteSpace())
            {
                if (useDocker)
                {
                    const string DockerTmpCodeDir = "/tmp/code";
                    var additonalDockerVolumn = string.Empty;
                    if (Environment.OSVersion.Platform != PlatformID.Win32NT
                        && !langEnvConfig.DockerVolumn.IsEmptyOrWhiteSpace())
                    {
                        additonalDockerVolumn = $"-v {langEnvConfig.DockerVolumn}";
                    }

                    buildCommand = $"docker run --rm {additonalDockerVolumn} -v {tmpDir.FullPath}:{DockerTmpCodeDir} -w {DockerTmpCodeDir} {docker} {buildCommand.WrapCommandWithSh()}";
                }

                await ProcessUtils.RunCommandAsync(
                    buildCommand,
                    workingDir: tmpDir.FullPath).ConfigureAwait(false);
            }

            // After Build
            await ProcessUtils.RunCommandsAsync(
                langEnvConfig.AfterBuild,
                workingDir: tmpDir.FullPath).ConfigureAwait(false);

            if (Directory.Exists(buildOutput))
            {
                Directory.Delete(buildOutput, recursive: true);
            }

            try
            {
                if (tmpBuildOutput.IsDirectoryNotEmpty())
                {
                    Logger.Debug($"Moving from {tmpBuildOutput} to {buildOutput}");
                    Directory.Move(tmpBuildOutput, buildOutput);
                    Logger.Debug($"Moved from {tmpBuildOutput} to {buildOutput}");
                }
            }
            catch (IOException ioe) when (ioe.Message.Contains("Invalid cross-device link", StringComparison.OrdinalIgnoreCase))
            {
                await ProcessUtils.RunCommandAsync($"cp -a \"{tmpBuildOutput}\" \"{buildOutput}\"", asyncRead: false).ConfigureAwait(false);
                Logger.Debug($"Copied from {tmpBuildOutput} to {buildOutput}");
            }

            if (!buildOutput.IsDirectoryNotEmpty())
            {
                throw new DirectoryNotFoundException(buildOutput);
            }

            if (_verbose)
            {
                await ProcessUtils.RunCommandAsync($"ls -al {buildOutput}", asyncRead: false).ConfigureAwait(false);
            }
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

            await ProcessUtils.RunCommandsAsync(langEnvConfig.BeforeRun, workingDir: buildOutput).ConfigureAwait(false);

            var exeName = Path.Combine(buildOutput, langEnvConfig.RunCmd.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0]);
            await ProcessUtils.RunCommandAsync($"chmod +x \"{exeName}\"", asyncRead: false, workingDir: buildOutput).ConfigureAwait(false);

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

                // Test retry
                Exception error = null;
                for (var retry = 0; retry < 2; retry++)
                {
                    ProcessUtils.RunProcess(
                        runPsi,
                        printOnConsole: false,
                        asyncRead: false,
                        out var stdOut,
                        out var stdErr,
                        default);
                    if (StringComparer.Ordinal.Equals(expectedOutput.TrimEnd(), stdOut.TrimEnd()))
                    {
                        Logger.Info($"Test Passed: {buildId}");
                        error = null;
                        break;
                    }
                    else
                    {
                        error = new Exception($"Test Failed: {buildId}"
                            + $"\nInput: {test.Input}"
                            + $"\nExpected output path: {expectedOutputPath}"
                            + $"\n Output: {stdOut}"
                            + $"\n Expected output: {expectedOutput}");
                    }
                }

                if (error != null)
                {
                    throw error;
                }
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

            await ProcessUtils.RunCommandsAsync(langEnvConfig.BeforeRun, workingDir: buildOutput).ConfigureAwait(false);

            var exeName = Path.Combine(buildOutput, langEnvConfig.RunCmd.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0]);
            var problemTestConfig = benchConfig.Problems.FirstOrDefault(i => i.Name == problem.Name);
            foreach (var test in problemTestConfig.Tests)
            {
                if (test.SkipOnPullRequest && AppveyorUtils.IsPullRequest)
                {
                    continue;
                }

                var runCommand = $"{langEnvConfig.RunCmd} {test.Input}";

                var runPsi = runCommand.ConvertToCommand();
                runPsi.FileName = exeName;
                runPsi.WorkingDirectory = buildOutput;

                var repeat = test.Repeat > 1 ? test.Repeat : 1;
                var measurements = new List<ProcessMeasurement>(repeat);
                for (var i = 0; i < repeat; i++)
                {
                    var measurement = await ProcessUtils.MeasureAsync(runPsi).ConfigureAwait(false);
                    Logger.Debug($"{buildId} {measurement}");
                    measurements.Add(measurement);
                }

                var avgMeasurement = measurements.GetAverage();
                Logger.Info($"\n\n[AVG] {buildId} {avgMeasurement}\n\n");

                var benchResultJsonPath = Path.Combine(benchResultDir, $"{buildId}_{test.Input}.json");
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
                    appveyorBuildId = AppveyorUtils.BuildId,
                }, Formatting.Indented)).ConfigureAwait(false);
            }
        }
    }
}
