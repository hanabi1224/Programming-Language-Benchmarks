using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Win32.SafeHandles;
using NLog;

namespace BenchTool
{
    public static class ProcessMeasurementExtensions
    {
        public static ProcessMeasurement GetAverage(this ICollection<ProcessMeasurement> array)
        {
            if (array.Count == 0)
            {
                return new ProcessMeasurement();
            }
            else if (array.Count == 1)
            {
                return array.Single();
            }

            return new ProcessMeasurement
            {
                Elapsed = TimeSpan.FromMilliseconds(array.Average(i => i.Elapsed.TotalMilliseconds)),
                CpuTime = TimeSpan.FromMilliseconds(array.Average(i => i.CpuTime.TotalMilliseconds)),
                PeakMemoryBytes = (long)array.Average(i => i.PeakMemoryBytes),
            };
        }
    }

    public class ProcessMeasurement
    {
        public TimeSpan Elapsed { get; set; }

        public TimeSpan CpuTime { get; set; }

        public long PeakMemoryBytes { get; set; }

        public override string ToString()
        {
            return $"[{Environment.ProcessorCount} cores]time: {Elapsed.TotalMilliseconds}ms, cpu-time: {CpuTime.TotalMilliseconds}ms, peak-mem: {PeakMemoryBytes / 1024}KB";
        }
    }

    public static class ProcessUtils
    {
        private static Logger Logger { get; } = LogManager.GetCurrentClassLogger();

        public static async Task<ProcessMeasurement> MeasureAsync(
            ProcessStartInfo startInfo,
            int sampleIntervalMS = 5,
            CancellationToken token = default)
        {
            var m = new ProcessMeasurement();
            startInfo.UseShellExecute = false;
            var p = new Process
            {
                StartInfo = startInfo,
            };

            var started = false;
            var t = Task.Factory.StartNew(() =>
            {
                while (!started)
                {
                    Thread.Sleep(1);
                }

                IList<Process> childrenProcesses = null;

                var nLoop = 0;
                while (true)
                {
                    try
                    {
                        p.Refresh();
                        m.CpuTime = p.TotalProcessorTime;
                        var totalMemoryBytes = p.WorkingSet64;
                        if (childrenProcesses?.Count > 0)
                        {
                            foreach (var cp in childrenProcesses)
                            {
                                try
                                {
                                    if (!cp.HasExited)
                                    {
                                        cp.Refresh();
                                        m.CpuTime += cp.TotalProcessorTime;
                                        totalMemoryBytes += cp.WorkingSet64;
                                    }
                                }
                                catch (Exception e)
                                {
                                    Logger.Error(e);
                                }
                            }
                        }

                        if (totalMemoryBytes > m.PeakMemoryBytes)
                        {
                            m.PeakMemoryBytes = totalMemoryBytes;
                        }

                        // Look for children process after first recording
                        if (childrenProcesses == null)
                        {
                            // Prevent 2nd check
                            childrenProcesses = new List<Process>();

                            // Looking for child processes
                            var stdoutBuilder = new StringBuilder();
                            RunCommandAsync(command: $"pgrep -P {p.Id}", stdOutBuilder: stdoutBuilder).Wait();
                            var stdout = stdoutBuilder.ToString().Trim();
                            if (!stdout.IsEmptyOrWhiteSpace())
                            {
                                var matches = Regex.Matches(stdout, @"\d+", RegexOptions.Compiled);
                                if (matches?.Count > 0)
                                {
                                    foreach (Match m in matches)
                                    {
                                        var childPid = int.Parse(m.Value);
                                        childrenProcesses.Add(Process.GetProcessById(childPid));
                                    }
                                }
                            }
                        }
                        else if (nLoop < 10 || m.PeakMemoryBytes <= 0)
                        {
                            Thread.Sleep(1);
                        }
                        else
                        {
                            Thread.Sleep(sampleIntervalMS);
                        }

                        if (p.HasExited)
                        {
                            return;
                        }

                        nLoop++;
                    }
                    catch (InvalidOperationException)
                    {
                        return;
                    }
                    catch (Exception e)
                    {
                        Logger.Error(e);
                        return;
                    }
                }
            }, TaskCreationOptions.LongRunning);

            var sw = Stopwatch.StartNew();
            RunProcess(
                p,
                printOnConsole: false,
                asyncRead: true,
                stdOutBuilder: null,
                stdErrorBuilder: null,
                token,
                onStart: () => started = true);

            sw.Stop();
            await t.ConfigureAwait(false);
            m.Elapsed = sw.Elapsed;
            return m;
        }

        public static async Task RunCommandsAsync(
            IEnumerable<string> commands,
            string workingDir = null,
            bool asyncRead = false,
            bool ensureZeroExitCode = false,
            CancellationToken token = default
            )
        {
            if (commands == null)
            {
                return;
            }

            foreach (var command in commands)
            {
                await RunCommandAsync(
                    command: command,
                    workingDir: workingDir,
                    asyncRead: asyncRead,
                    ensureZeroExitCode: ensureZeroExitCode,
                    token: token).ConfigureAwait(false);
            }
        }

        public static async Task RunCommandAsync(
            string command,
            string workingDir = null,
            bool asyncRead = false,
            bool ensureZeroExitCode = false,
            StringBuilder stdOutBuilder = null,
            StringBuilder stdErrorBuilder = null,
            CancellationToken token = default)
        {
            if (workingDir.IsEmptyOrWhiteSpace())
            {
                workingDir = Environment.CurrentDirectory;
            }

            await Task.Delay(1).ConfigureAwait(false);

            var psi = command.ConvertToCommand();
            psi.WorkingDirectory = workingDir;

            var ret = RunProcess(
                psi,
                useShellExecute: false,
                printOnConsole: true,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token);

            if (ensureZeroExitCode && ret != 0)
            {
                throw new InvalidOperationException($"[Non zero exit code {ret}] {command}");
            }
        }

        public static int RunProcess(
                ProcessStartInfo startInfo,
                bool printOnConsole,
                bool asyncRead,
                out string stdOut,
                out string stdError,
                CancellationToken token)
        {
            var stdOutBuilder = new StringBuilder();
            var stdErrorBuilder = new StringBuilder();

            var ret = RunProcess(
                startInfo: startInfo,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token);

            stdOut = stdOutBuilder.ToString();
            stdError = stdErrorBuilder.ToString();

            return ret;
        }

        public static int RunProcess(
            ProcessStartInfo startInfo,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            bool printOnConsole,
            bool asyncRead,
            CancellationToken token)
        {
            return RunProcess(
                startInfo: startInfo,
                useShellExecute: false,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token);
        }

        public static int RunProcess(
            ProcessStartInfo startInfo,
            bool useShellExecute,
            bool printOnConsole,
            bool asyncRead,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            CancellationToken token)
        {
            startInfo.UseShellExecute = useShellExecute;

            var p = new Process
            {
                StartInfo = startInfo,
            };

            return RunProcess(
                p: p,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token);
        }

        public static int RunProcess(
            Process p,
            bool printOnConsole,
            bool asyncRead,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            CancellationToken token,
            Action onStart = null)
        {
            using (p)
            {
                var useShellExecute = p.StartInfo.UseShellExecute;
                p.StartInfo.RedirectStandardOutput = !useShellExecute;
                p.StartInfo.RedirectStandardError = !useShellExecute;

                var prefix = $"Command[shell:{useShellExecute},print:{printOnConsole},async:{asyncRead}]:";
                Logger.Debug($"{prefix}: {p.StartInfo.FileName} {p.StartInfo.Arguments}");

                if (p.StartInfo.RedirectStandardOutput)
                {
                    p.StartInfo.StandardOutputEncoding = Encoding.UTF8;
                    p.OutputDataReceived += (object sender, DataReceivedEventArgs e) =>
                    {
                        stdOutBuilder?.AppendLine(e.Data);
                        if (printOnConsole)
                        {
                            if (e.Data.IsEmptyOrWhiteSpace())
                            {
                                Console.WriteLine(e.Data);
                            }
                            else
                            {
                                Logger.Trace(e.Data);
                            }
                        }
                    };
                }

                if (p.StartInfo.RedirectStandardError)
                {
                    p.StartInfo.StandardErrorEncoding = Encoding.UTF8;
                    p.ErrorDataReceived += (object sender, DataReceivedEventArgs e) =>
                    {
                        stdErrorBuilder?.AppendLine(e.Data);
                        if (printOnConsole)
                        {
                            if (e.Data.IsEmptyOrWhiteSpace())
                            {
                                Console.Error.WriteLine(e.Data);
                            }
                            else
                            {
                                Logger.Error(e.Data);
                            }
                        }
                    };
                }

                p.Start();
                onStart?.Invoke();
                if (asyncRead)
                {
                    try
                    {
                        if (p.StartInfo.RedirectStandardOutput)
                        {
                            p.BeginOutputReadLine();
                        }
                        if (p.StartInfo.RedirectStandardError)
                        {
                            p.BeginErrorReadLine();
                        }
                    }
                    catch (Exception e)
                    {
                        Logger.Error(e.Message);
                    }
                }
                else
                {
                    // Avoid deadlock in sync mode
                    Task.Run(async () =>
                    {
                        await Task.Delay(1000).ConfigureAwait(false);
                        try
                        {
                            if (!p.HasExited)
                            {
                                if (p.StartInfo.RedirectStandardOutput)
                                {
                                    p.BeginOutputReadLine();
                                }
                                if (p.StartInfo.RedirectStandardError)
                                {
                                    p.BeginErrorReadLine();
                                }
                            }
                        }
                        catch (InvalidOperationException)
                        {
                        }
                        catch (Exception e)
                        {
                            Logger.Error(e.Message);
                        }
                    });
                }

                using (var processEnded = new ManualResetEvent(false))
                {
                    using var safeWaitHandle = new SafeWaitHandle(p.Handle, false);

                    processEnded.SetSafeWaitHandle(safeWaitHandle);

                    var index = WaitHandle.WaitAny(new[] { processEnded, token.WaitHandle });

                    //If the signal came from the caller cancellation token close the window
                    if (index == 1
                        && !p.HasExited)
                    {
                        p.CloseMainWindow();
                        p.Kill();
                        return -1;
                    }
                    else if (index == 0 && !p.HasExited)
                    {
                        // Workaround for linux: https://github.com/dotnet/corefx/issues/35544
                        p.WaitForExit();
                    }
                }

                try
                {
                    if (p.StartInfo.RedirectStandardOutput)
                    {
                        p.StandardOutput.BaseStream.Flush();
                        var outRm = p.StandardOutput.ReadToEnd();
                        if (!outRm.IsEmptyOrWhiteSpace())
                        {
                            stdOutBuilder?.Append(outRm);
                            if (printOnConsole)
                            {
                                Logger.Trace(outRm);
                            }
                        }
                    }
                    if (p.StartInfo.RedirectStandardError)
                    {
                        p.StandardError.BaseStream.Flush();
                        var errRm = p.StandardError.ReadToEnd();
                        if (!errRm.IsEmptyOrWhiteSpace())
                        {
                            stdErrorBuilder?.Append(errRm);
                            if (printOnConsole)
                            {
                                Logger.Error(errRm);
                            }
                        }
                    }

                    return p.ExitCode;
                }
                catch (InvalidOperationException)
                {
                    return -1;
                }
            }
        }

        /// <summary>
        /// https://github.com/dotnet/corefx/issues/35544
        /// </summary>
        public static void BugRepro()
        {
            Process p;
            if (Environment.OSVersion.Platform == PlatformID.Win32NT)
            {
                p = Process.Start("timeout", "10");
            }
            else
            {
                p = Process.Start("sleep", "10s");
            }

            var waitHandle = new ManualResetEvent(false);
            waitHandle.SetSafeWaitHandle(new SafeWaitHandle(p.Handle, false));
            WaitHandle.WaitAll(new[] { waitHandle });
            if (!p.HasExited)
            {
                throw new Exception("Process wait handle is not working properly");
            }
            else
            {
                Console.WriteLine("success");
            }
        }
    }
}
