using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using Newtonsoft.Json;
using NLog;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace BenchTool
{
    public static class Program
    {
        private static Logger Logger { get; } = LogManager.GetCurrentClassLogger();

        private const string TimerPrefix = "[Timer] ";

        static Program()
        {
            NLogUtils.Configure();
        }

        private const string TaskBuild = "build";
        private const string TaskTest = "test";
        private const string TaskBench = "bench";

        private static bool s_verbose = false;
        private static CpuInfo s_cpuInfo;

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
        /// <param name="buildPool">A flag that indicates whether builds that can run in parallel</param>
        /// <param name="verbose">A Flag that indicates whether to print verbose infomation</param>
        /// <param name="noDocker">A Flag that forces disabling docker</param>
        /// <param name="ignoreMissing">A Flag that indicates whether to ignore test/bench failure when build artifacts is missing</param>
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
            bool buildPool = false,
            bool verbose = false,
            bool noDocker = false,
            bool ignoreMissing = false,
            string[] langs = null,
            string[] problems = null,
            string[] environments = null)
        {
            s_cpuInfo = await CpuInfo.LsCpuAsync().ConfigureAwait(false);

            Stopwatch timer = Stopwatch.StartNew();
            s_verbose = verbose;
            config.EnsureFileExists();
            algorithm.EnsureDirectoryExists();
            include.EnsureDirectoryExists();

            buildOutput.CreateDirectoryIfNotExist();

            if (!new HashSet<string> { TaskBuild, TaskTest, TaskBench }.Contains(task))
            {
                throw new NotSupportedException($"Unknown task: {task}");
            }

            IDeserializer yamlDeserializer = new DeserializerBuilder()
                .WithNamingConvention(UnderscoredNamingConvention.Instance)
                .Build();

            string yamlStr = File.ReadAllText(config);
            YamlBenchmarkConfig benchConfig = yamlDeserializer.Deserialize<YamlBenchmarkConfig>(yamlStr);
            if (benchConfig.Langs == null)
            {
                benchConfig.Langs = new List<YamlLangConfig>();
            }
            string configDir = Path.GetDirectoryName(config);
            foreach (string lcPath in Directory.GetFiles(configDir.FallBackTo("."), "bench_*.yaml", SearchOption.TopDirectoryOnly))
            {
                Logger.Debug($"Loading {lcPath}");
                YamlLangConfig lc = yamlDeserializer.Deserialize<YamlLangConfig>(File.ReadAllText(lcPath));
                benchConfig.Langs.Add(lc);
            }

            List<YamlLangConfig> langConfigs = benchConfig.Langs;
            HashSet<string> includedLanguages = new HashSet<string>(langs ?? new string[] { }, StringComparer.OrdinalIgnoreCase);
            HashSet<string> includedOsEnvironments = new HashSet<string>(environments ?? new string[] { }, StringComparer.OrdinalIgnoreCase);
            HashSet<string> includedProblems = new HashSet<string>(problems ?? new string[] { }, StringComparer.OrdinalIgnoreCase);

            if (s_cpuInfo != null)
            {
                Logger.Info($"CPU: {s_cpuInfo}");
                if (GithubActionUtils.IsGithubBuild && task == TaskBench && s_cpuInfo.Model < 79)
                {
                    // To print cpu features, use
                    // either: 'rustc +nightly --print=cfg -C target-cpu=broadwell' (features like avx512 are missing from stable)
                    // or 'zig build -Dcpu=broadwell --verbose-llvm-cpu-features'
                    throw new Exception("[github action] Fail intentionally on old cpu model prior to broadwell, please retry.");
                }
            }

            List<Task> parallelTasks = new List<Task>();
            List<Exception> aggregatedExceptions = new List<Exception>();
            HashSet<string> SetupDockerProvidedRuntimeDedupContext = new HashSet<string>();
            foreach (YamlLangConfig c in langConfigs.OrderBy(i => i.Lang))
            {
                if (!c.Enabled)
                {
                    continue;
                }
                if (includedLanguages.Count > 0
                    && !includedLanguages.Contains(c.Lang))
                {
                    continue;
                }

                Logger.Info($"[{c.Lang}] Running task {task}...");
                Stopwatch langTaskTimer = Stopwatch.StartNew();

                foreach (YamlLangEnvironmentConfig env in c.Environments ?? Enumerable.Empty<YamlLangEnvironmentConfig>())
                {
                    if (!env.Enabled)
                    {
                        continue;
                    }
                    if (includedOsEnvironments.Count > 0
                        && !includedOsEnvironments.Contains(env.Os))
                    {
                        continue;
                    }

                    if (c.Problems == null)
                    {
                        continue;
                    }

                    foreach (YamlLangProblemConfig p in c.Problems.OrderBy(_ => _.Name))
                    {
                        if (includedProblems.Count > 0
                            && !includedProblems.Contains(p.Name))
                        {
                            continue;
                        }

                        foreach (string codePath in p.Source ?? Enumerable.Empty<string>())
                        {
                            if (!noDocker && task == TaskBuild)
                            {
                                await SetupDockerProvidedRuntimeAsync(langEnvConfig: env, buildOutputRoot: buildOutput, dedupContext: SetupDockerProvidedRuntimeDedupContext).ConfigureAwait(false);
                            }

                            bool allowParallel = task == TaskBuild && buildPool;
                            Task rawJobExecutionTask = null;
                            string buildId = $"{c.Lang}_{env.Os}_{env.Compiler}_{env.Version}_{env.CompilerOptionsText}_{p.Name}_{Path.GetFileNameWithoutExtension(codePath)}";
                            buildId = Regex.Replace(buildId, @"[\\\/\?]", "_", RegexOptions.Compiled);
                            Logger.Info($"Starting {task} task: {buildId}");
                            Stopwatch taskTimer = Stopwatch.StartNew();

                            switch (task)
                            {
                                case TaskBuild:
                                    rawJobExecutionTask = BuildAsync(buildId, benchConfig, c, env, p, codePath: codePath, algorithmDir: algorithm, buildOutputDir: buildOutput, includeDir: include, forcePullDocker: forcePullDocker, forceRebuild: forceRebuild, noDocker: noDocker);
                                    break;
                                case TaskTest:
                                    rawJobExecutionTask = TestAsync(buildId, benchConfig, c, env, p, algorithmDir: algorithm, buildOutputRoot: buildOutput, ignoreMissing: ignoreMissing);
                                    break;
                                case TaskBench:
                                    rawJobExecutionTask = BenchAsync(buildId, benchConfig, c, env, p, codePath: codePath, algorithmDir: algorithm, buildOutputRoot: buildOutput, ignoreMissing: ignoreMissing);
                                    break;
                                default:
                                    continue;
                            }

                            Task jobExecutionTask = Task.Run(async () =>
                            {
                                try
                                {
                                    await rawJobExecutionTask.ConfigureAwait(false);
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
                                finally
                                {
                                    taskTimer.Stop();
                                }
                            });

                            if (allowParallel)
                            {
                                parallelTasks.Add(jobExecutionTask);
                            }
                            else
                            {
                                await jobExecutionTask.ConfigureAwait(false);
                            }
                        }
                    }
                }

                langTaskTimer.Stop();
                Logger.Info($"[{c.Lang}] Task {task} has finished in {langTaskTimer.Elapsed}");
            }

            if (parallelTasks?.Count > 0)
            {
                await Task.WhenAll(parallelTasks).ConfigureAwait(false);
            }

            if (aggregatedExceptions?.Count > 0)
            {
                throw new AggregateException(aggregatedExceptions);
            }

            timer.Stop();
            Logger.Info($"{TimerPrefix}Task {task} has finished in {timer.Elapsed}");
        }

        private static async Task BuildAsync(
            string buildId,
            YamlBenchmarkConfig benchConfig,
            YamlLangConfig langConfig,
            YamlLangEnvironmentConfig langEnvConfig,
            YamlLangProblemConfig problem,
            string codePath,
            string algorithmDir,
            string buildOutputDir,
            string includeDir,
            bool forcePullDocker,
            bool forceRebuild,
            bool noDocker)
        {
            string buildOutput = Path.Combine(Environment.CurrentDirectory, buildOutputDir, buildId);
            if (!forceRebuild
                && buildOutput.IsDirectoryNotEmpty())
            {
                Logger.Debug($"Build cache hit.");
                return;
            }

            BuildOutputJson buildOutputJson = new BuildOutputJson();

            // Source code
            string srcCodePath = Path.Combine(algorithmDir, problem.Name, codePath);
            srcCodePath.EnsureFileExists();

            var tmpRoot = Path.Combine(Path.GetTempPath(), "__benchmarks");
            if (!Directory.Exists(tmpRoot))
            {
                Directory.CreateDirectory(tmpRoot);
            }

            // Setup tmp build folder
            using TempFolder tmpDir = new TempFolder(Path.Combine("__benchmarks", buildId));

            Logger.Debug($"Temp build folder: {tmpDir.FullPath}");

            // Copy Include folder
            if (!langEnvConfig.Include.IsEmptyOrWhiteSpace())
            {
                string fromDir = Path.Combine(includeDir, langEnvConfig.Include);
                fromDir.EnsureDirectoryExists();

                await ProcessUtils.RunCommandAsync($"cp -a \"{fromDir}\"  \"{tmpDir.FullPath}\"", asyncRead: false).ConfigureAwait(false);
            }

            string tmpBuildOutput = Path.Combine(tmpDir.FullPath, langEnvConfig.OutDir ?? string.Empty);
            tmpBuildOutput.CreateDirectoryIfNotExist();

            string srcCodeDestDir = langEnvConfig.IncludeSubDir.IsEmptyOrWhiteSpace() ? tmpDir.FullPath : Path.Combine(tmpDir.FullPath, langEnvConfig.IncludeSubDir);
            srcCodeDestDir.CreateDirectoryIfNotExist();

            string srcCodeDestFileName = langEnvConfig.SourceRenameTo
                .FallBackTo(langConfig.SourceRenameTo)
                .FallBackTo(Path.GetFileName(srcCodePath));
            string srcCodeDestPath = Path.Combine(srcCodeDestDir, srcCodeDestFileName);
            Logger.Debug($"Copying {srcCodePath} to {srcCodeDestPath}");
            //File.Copy(srcCodePath, srcCodeDestPath, overwrite: true);
            await File.WriteAllTextAsync(path: srcCodeDestPath, await File.ReadAllTextAsync(srcCodePath).ConfigureAwait(false)).ConfigureAwait(false);
            if (s_verbose)
            {
                await ProcessUtils.RunCommandAsync($"ls -al \"{tmpDir.FullPath}\"", asyncRead: false).ConfigureAwait(false);
            }

            // Docker setup
            string docker = langEnvConfig.Docker;
            bool useDocker = !noDocker && !docker.IsEmptyOrWhiteSpace();
            if (useDocker && forcePullDocker)
            {
                await ProcessUtils.RunCommandAsync($"docker pull {docker}").ConfigureAwait(false);
            }

            // Before Build
            await ProcessUtils.RunCommandsAsync(
                langEnvConfig.BeforeBuild,
                workingDir: tmpDir.FullPath).ConfigureAwait(false);

            // Check compiler version and save output
            string compilerVersionCommand = langEnvConfig.CompilerVersionCommand.FallBackTo(langConfig.CompilerVersionCommand);
            if (!compilerVersionCommand.IsEmptyOrWhiteSpace())
            {
                if (useDocker)
                {
                    const string DockerTmpCodeDir = "/tmp/code";
                    compilerVersionCommand = $"docker run --rm -v {tmpDir.FullPath}:{DockerTmpCodeDir} -w {DockerTmpCodeDir} {docker} {compilerVersionCommand}";
                }
                else
                {
                    await ProcessUtils.RunCommandAsync(
                       compilerVersionCommand,
                       workingDir: tmpDir.FullPath,
                       stdOutBuilder: null,
                       stdErrorBuilder: null).ConfigureAwait(false);
                }

                {
                    StringBuilder stdOutBuilder = new StringBuilder();
                    StringBuilder stdErrorBuilder = new StringBuilder();
                    await ProcessUtils.RunCommandAsync(
                        compilerVersionCommand,
                        workingDir: tmpDir.FullPath,
                        stdOutBuilder: stdOutBuilder,
                        stdErrorBuilder: stdErrorBuilder).ConfigureAwait(false);

                    buildOutputJson.CompilerVersionText = stdOutBuilder.ToString().Trim().FallBackTo(stdErrorBuilder.ToString().Trim());
                }

                if (buildOutputJson.CompilerVersionText?.Contains("Unable to find image", StringComparison.OrdinalIgnoreCase) == true)
                {
                    StringBuilder stdOutBuilder = new StringBuilder();
                    StringBuilder stdErrorBuilder = new StringBuilder();
                    await ProcessUtils.RunCommandAsync(
                        compilerVersionCommand,
                        workingDir: tmpDir.FullPath,
                        stdOutBuilder: stdOutBuilder,
                        stdErrorBuilder: stdErrorBuilder).ConfigureAwait(false);

                    buildOutputJson.CompilerVersionText = stdOutBuilder.ToString().Trim().FallBackTo(stdErrorBuilder.ToString().Trim());
                }
            }

            // Build
            string buildCommand = langEnvConfig.Build;
            if (!buildCommand.IsEmptyOrWhiteSpace())
            {
                if (useDocker)
                {
                    const string DockerTmpCodeDir = "/tmp/code";
                    string additonalDockerVolumn = string.Empty;
                    if (Environment.OSVersion.Platform != PlatformID.Win32NT
                        && langEnvConfig.DockerVolumns?.Length > 0)
                    {
                        additonalDockerVolumn = string.Join(" ", langEnvConfig.DockerVolumns.Select(v => $"-v {v}"));
                    }

                    string additionalDockerEnv = string.Empty;
                    if (langEnvConfig.Env?.Count > 0)
                    {
                        additionalDockerEnv = string.Join(" ", langEnvConfig.Env.Select(p => $"--env {p.Key.Trim()}=\"{p.Value.Trim()}\""));
                    }

                    buildCommand = $"docker run --rm {additonalDockerVolumn} {additionalDockerEnv} -v {tmpDir.FullPath}:{DockerTmpCodeDir} -w {DockerTmpCodeDir} {docker} {buildCommand.WrapCommandWithSh()}";
                    await ProcessUtils.RunCommandAsync(
                        buildCommand,
                        workingDir: tmpDir.FullPath,
                        env: langEnvConfig.Env).ConfigureAwait(false);
                }
                else
                {
                    await ProcessUtils.RunCommandsAsync(
                        buildCommand.Split("&&", StringSplitOptions.RemoveEmptyEntries),
                        workingDir: tmpDir.FullPath,
                        env: langEnvConfig.Env).ConfigureAwait(false);
                }
            }

            // After Build
            await ProcessUtils.RunCommandsAsync(
                langEnvConfig.AfterBuild,
                workingDir: tmpDir.FullPath).ConfigureAwait(false);

            await SetupTestData(
                benchConfig.Problems.FirstOrDefault(i => i.Name == problem.Name),
                algorithmDir: algorithmDir,
                tmpBuildOutput: tmpBuildOutput).ConfigureAwait(false);

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
                if (langEnvConfig.AllowFailure)
                {
                    Logger.Warn($"Build failed, but failure is configured to be ignored");
                    if (Directory.Exists(buildOutput))
                    {
                        Directory.Delete(buildOutput, recursive: true);
                    }
                    return;
                }

                throw new DirectoryNotFoundException(buildOutput);
            }

            await buildOutputJson.SaveAsync(buildOutput).ConfigureAwait(false);

            if (s_verbose)
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
            string buildOutputRoot,
            bool ignoreMissing)
        {
            string buildOutput = Path.Combine(Environment.CurrentDirectory, buildOutputRoot, buildId);
            if (!Directory.Exists(buildOutput))
            {
                if (ignoreMissing || langEnvConfig.AllowFailure)
                {
                    return;
                }
            }
            buildOutput.EnsureDirectoryExists();

            TestOutputJson testOutputJson = new TestOutputJson();

            await ProcessUtils.RunCommandsAsync(langEnvConfig.BeforeRun, workingDir: buildOutput).ConfigureAwait(false);

            string exeName = langEnvConfig.RunCmd.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0];
            if (langEnvConfig.RuntimeIncluded)
            {
                string exeRoot = GetIncludedRuntimeRoot(langEnvConfig: langEnvConfig, buildOutputRoot: buildOutputRoot, buildOutput: buildOutput);
                exeName = Path.Combine(exeRoot, exeName);
                await ProcessUtils.RunCommandAsync($"chmod +x \"{exeName}\"", asyncRead: false, workingDir: buildOutput).ConfigureAwait(false);
            }

            string runtimeVersionParameter = langEnvConfig.RuntimeVersionParameter.FallBackTo(langConfig.RuntimeVersionParameter);
            if (!runtimeVersionParameter.IsEmptyOrWhiteSpace())
            {
                StringBuilder stdOutBuilder = new StringBuilder();
                StringBuilder stdErrorBuilder = new StringBuilder();
                await ProcessUtils.RunCommandAsync(
                    $"{exeName} {runtimeVersionParameter}",
                    workingDir: buildOutput,
                    stdOutBuilder: stdOutBuilder,
                    stdErrorBuilder: stdErrorBuilder).ConfigureAwait(false);

                testOutputJson.RuntimeVersionText = stdOutBuilder.ToString().Trim().FallBackTo(stdErrorBuilder.ToString().Trim());
            }

            YamlBenchmarkProblemConfig problemTestConfig = benchConfig.Problems.FirstOrDefault(i => i.Name == problem.Name);
            foreach (YamlBenchmarkProblemUnittestConfig test in problemTestConfig.Unittests)
            {
                string expectedOutputPath = Path.Combine(algorithmDir, problem.Name, test.Output);
                expectedOutputPath.EnsureFileExists();

                string expectedOutput = File.ReadAllText(expectedOutputPath);

                string runCommand = $"{langEnvConfig.RunCmd} {test.Input}";

                ProcessStartInfo runPsi = runCommand.ConvertToCommand();
                runPsi.FileName = exeName;
                runPsi.WorkingDirectory = buildOutput;

                // Test retry
                Exception error = null;
                for (int retry = 0; retry < 3; retry++)
                {
                    using var cts = new CancellationTokenSource();
                    cts.CancelAfter(TimeSpan.FromSeconds(60));
                    ProcessUtils.RunProcess(
                        runPsi,
                        printOnConsole: false,
                        asyncRead: false,
                        out string stdOut,
                        out string stdErr,
                        env: langEnvConfig.RunCmdEnv,
                        token: cts.Token);
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
                            + $"\n Std Out:\n{stdOut}"
                            + $"\n Std Err:\n{stdErr}"
                            + $"\n Expected output:\n{expectedOutput}");
                    }
                }

                if (error != null)
                {
                    await ProcessUtils.RunProcessAsync(
                        runPsi,
                        useShellExecute: true,
                        printOnConsole: false,
                        asyncRead: false,
                        stdOutBuilder: null,
                        stdErrorBuilder: null,
                        env: null,
                        default);
                    throw error;
                }
            }

            await testOutputJson.SaveAsync(buildOutput).ConfigureAwait(false);
        }

        private static async Task BenchAsync(
                string buildId,
                YamlBenchmarkConfig benchConfig,
                YamlLangConfig langConfig,
                YamlLangEnvironmentConfig langEnvConfig,
                YamlLangProblemConfig problem,
                string codePath,
                string algorithmDir,
                string buildOutputRoot,
                bool ignoreMissing)
        {
            string buildOutput = Path.Combine(Environment.CurrentDirectory, buildOutputRoot, buildId);
            if (!Directory.Exists(buildOutput))
            {
                if (ignoreMissing || langEnvConfig.AllowFailure)
                {
                    return;
                }
            }
            buildOutput.EnsureDirectoryExists();

            string benchResultDir = Path.Combine(Environment.CurrentDirectory, buildOutputRoot, "_results", langConfig.Lang);
            benchResultDir.CreateDirectoryIfNotExist();

            await ProcessUtils.RunCommandsAsync(langEnvConfig.BeforeRun, workingDir: buildOutput).ConfigureAwait(false);

            string exeName = langEnvConfig.RunCmd.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0];
            if (langEnvConfig.RuntimeIncluded)
            {
                string exeRoot = GetIncludedRuntimeRoot(langEnvConfig: langEnvConfig, buildOutputRoot: buildOutputRoot, buildOutput: buildOutput);
                exeName = Path.Combine(exeRoot, exeName);
                await ProcessUtils.RunCommandAsync($"chmod +x \"{exeName}\"", asyncRead: false, workingDir: buildOutput).ConfigureAwait(false);
            }

            YamlBenchmarkProblemConfig problemTestConfig = benchConfig.Problems.FirstOrDefault(i => i.Name == problem.Name);
            if (langEnvConfig.Warmup)
            {
                string runCommand = $"{langEnvConfig.RunCmd} {problemTestConfig.Unittests[0].Input}";
                ProcessStartInfo warmupPsi = runCommand.ConvertToCommand();
                warmupPsi.FileName = exeName;
                warmupPsi.WorkingDirectory = buildOutput;
                await ProcessUtils.RunProcessAsync(
                    startInfo: warmupPsi,
                    useShellExecute: true,
                    printOnConsole: false,
                    asyncRead: false,
                    stdOutBuilder: null,
                    stdErrorBuilder: null,
                    env: null,
                    default).ConfigureAwait(false);
                Logger.Debug($"Warmup finished. Command: {runCommand}");
            }
            
            foreach (YamlBenchmarkProblemTestConfig test in problemTestConfig.Tests)
            {
                if (test.SkipOnPullRequest
                    && GithubActionUtils.IsPullRequest)
                {
                    continue;
                }

                if (test.ExcludeLangs?.Count > 0
                    && test.ExcludeLangs.Contains(langConfig.Lang))
                {
                    continue;
                }

                string runCommand = $"{langEnvConfig.RunCmd} {test.Input}";

                ProcessStartInfo runPsi = runCommand.ConvertToCommand();
                runPsi.FileName = exeName;
                runPsi.WorkingDirectory = buildOutput;

                int repeat = test.Repeat > 1 ? test.Repeat : 1;

                // Speed up PR build
                if (repeat > 1 && GithubActionUtils.IsPullRequest)
                {
                    Logger.Debug($"Reseting repeat from {repeat} to 1 in PR build.");
                    repeat = 1;
                }

                ProcessMeasurement statsMeasurement = new ProcessMeasurement();
                for (int nRetry = 0; nRetry < 5; nRetry++)
                {
                    List<ProcessMeasurement> measurements = new List<ProcessMeasurement>(repeat);
                    int maxRetries = 10;
                    for (int i = 0; i < repeat && maxRetries > 0; i++)
                    {
                        try
                        {
                            ProcessMeasurement measurement = await ProcessUtils.MeasureAsync(
                                runPsi,
                                forceCheckChildProcesses: langEnvConfig.ForceCheckChildProcesses,
                                timeoutSeconds: test.TimeoutSeconds,
                                env: langEnvConfig.RunCmdEnv).ConfigureAwait(false);

                            if (measurement != null && measurement.Elapsed.TotalMilliseconds > 0)
                            {
                                Logger.Debug($"({buildId}){langConfig.Lang}:{problem.Name}:{test.Input} {measurement}");
                                measurements.Add(measurement);
                            }
                            else
                            {
                                measurements.Clear();
                                break;
                            }
                        }
                        catch (Exception e)
                        {
                            Logger.Error(e);
                            i--;
                            maxRetries--;
                        }
                    }
                    if (measurements.Count < 1)
                    {
                        break;
                    }

                    statsMeasurement = measurements.GetAverageStats();
                    Logger.Info($"\n[AVG] ({buildId}){langConfig.Lang}:{problem.Name}:{test.Input} {statsMeasurement}\n");
                    if (statsMeasurement.ElapsedStdDevMS < 30)
                    {
                        break;
                    }
                    else if (statsMeasurement.ElapsedStdDevMS < 60
                        && statsMeasurement.ElapsedStdDevMS * 2 < statsMeasurement.Elapsed.TotalMilliseconds)
                    {
                        break;
                    }
                    else if (statsMeasurement.ElapsedStdDevMS * 4 < statsMeasurement.Elapsed.TotalMilliseconds)
                    {
                        break;
                    }
                    Logger.Warn("Standard deviation is too large, retrying in 5s...");
                    await Task.Delay(TimeSpan.FromSeconds(5)).ConfigureAwait(false);
                }

                if (statsMeasurement.Elapsed.TotalMilliseconds > 0)
                {
                    string benchResultJsonPath = Path.Combine(benchResultDir, $"{buildId}_{test.Input}.json");
                    await File.WriteAllTextAsync(benchResultJsonPath, JsonConvert.SerializeObject(new
                    {
                        cpuInfo = s_cpuInfo?.ToString(),
                        lang = langConfig.Lang,
                        os = langEnvConfig.Os,
                        compiler = langEnvConfig.Compiler,
                        compilerVersion = langEnvConfig.Version,
                        test = problem.Name,
                        code = codePath,
                        input = test.Input,
                        timeMS = statsMeasurement.Elapsed.TotalMilliseconds,
                        timeStdDevMS = statsMeasurement.ElapsedStdDevMS,
                        memBytes = statsMeasurement.PeakMemoryBytes,
                        cpuTimeMS = statsMeasurement.CpuTime.TotalMilliseconds,
                        cpuTimeUserMS = statsMeasurement.CpuTimeUser.TotalMilliseconds,
                        cpuTimeKernelMS = statsMeasurement.CpuTimeKernel.TotalMilliseconds,
                        //appveyorBuildId = AppveyorUtils.BuildId,
                        githubRunId = GithubActionUtils.RunId,
                        buildLog = BuildOutputJson.LoadFrom(buildOutput),
                        testLog = TestOutputJson.LoadFrom(buildOutput),
                    }, Formatting.Indented)).ConfigureAwait(false);
                }
                else
                {
                    Logger.Error("No valid benchmark results is produced.");
                }
            }
        }

        private static async Task SetupTestData(
            YamlBenchmarkProblemConfig problemConfig,
            string algorithmDir,
            string tmpBuildOutput)
        {
            //YamlBenchmarkProblemConfig problemTestConfig = benchConfig.Problems.FirstOrDefault(i => i.Name == problem.Name);
            if (problemConfig.Data?.Length > 0)
            {
                foreach (string fileName in problemConfig.Data)
                {
                    string dataFileFromPath = Path.Combine(algorithmDir, problemConfig.Name, fileName);
                    string dataFileToPath = Path.Combine(tmpBuildOutput, Path.GetFileName(fileName));
                    Logger.Debug($"Copying {dataFileFromPath} to {dataFileToPath}");
                    File.Copy(dataFileFromPath, dataFileToPath, overwrite: true);
                }
                if (problemConfig.DataSetupCmd?.Length > 0)
                {
                    await ProcessUtils.RunCommandsAsync(problemConfig.DataSetupCmd, workingDir: tmpBuildOutput).ConfigureAwait(false);
                }
            }
        }

        private static async Task SetupDockerProvidedRuntimeAsync(
            YamlLangEnvironmentConfig langEnvConfig,
            string buildOutputRoot,
            HashSet<string> dedupContext)
        {
            string dir = GetIncludedRuntimeRoot(langEnvConfig: langEnvConfig, buildOutputRoot: buildOutputRoot, buildOutput: null);
            if (string.IsNullOrEmpty(dir)
                || dedupContext.Contains(dir))
            {
                return;
            }
            if (Directory.Exists(dir))
            {
                try
                {
                    Directory.Delete(dir, recursive: true);
                }
                catch (Exception e)
                {
                    Logger.Error(e);
                }
            }
            if (!Directory.Exists(dir))
            {
                Directory.CreateDirectory(dir);
            }
            const string DockerTmpDir = "/tmp/runtime";
            string cmd = $"docker run --rm -v {dir}:{DockerTmpDir} -w {DockerTmpDir} {langEnvConfig.Docker}";
            if (!string.IsNullOrEmpty(langEnvConfig.DockerRuntimeDir))
            {
                cmd = $"{cmd} cp -a {langEnvConfig.DockerRuntimeDir} .";
            }
            else
            {
                cmd = $"{cmd} cp  {langEnvConfig.DockerRuntimeFile} .";
            }
            await ProcessUtils.RunCommandAsync(cmd).ConfigureAwait(false);
            dedupContext.Add(dir);
        }

        private static string GetIncludedRuntimeRoot(
            YamlLangEnvironmentConfig langEnvConfig,
            string buildOutputRoot,
            string buildOutput)
        {
            if (!string.IsNullOrEmpty(langEnvConfig.Docker))
            {
                if (!string.IsNullOrEmpty(langEnvConfig.DockerRuntimeDir)
                    || !string.IsNullOrEmpty(langEnvConfig.DockerRuntimeFile))
                {
                    string dir = Path.Combine(buildOutputRoot, "_runtimes_", GetDirNameFromDockerName(langEnvConfig.Docker));
                    // Make dir absolute
                    if (!dir.StartsWith('/') && !dir.Contains(":\\"))
                    {
                        dir = Path.Combine(Environment.CurrentDirectory, dir);
                    }
                    return dir;
                }
            }
            return buildOutput;
        }

        private static string GetDirNameFromDockerName(string docker)
        {
            return Regex.Replace(docker, @"[:/\\]", "_");
        }
    }
}
