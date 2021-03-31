using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Win32.SafeHandles;
using NLog;
using static Interop;

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
        private static readonly bool s_isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows);

        private static Logger Logger { get; } = LogManager.GetCurrentClassLogger();

        private static long s_ticksPerSecond;

        static ProcessUtils()
        {
            if (!s_isWindows)
            {
                // Look up the number of ticks per second in the system's configuration,
                // then use that to convert to a TimeSpan
                var ticksPerSecond = Interop.Sys.SysConf(Interop.Sys.SysConfName._SC_CLK_TCK);
                if (ticksPerSecond <= 0)
                {
                    throw new Win32Exception();
                }

                Volatile.Write(ref s_ticksPerSecond, ticksPerSecond);
            }
        }

        /// <summary>Convert a number of "jiffies", or ticks, to a TimeSpan.</summary>
        /// <param name="ticks">The number of ticks.</param>
        /// <returns>The equivalent TimeSpan.</returns>
        internal static TimeSpan TicksToTimeSpanLinux(double ticks)
        {
            return TimeSpan.FromSeconds(ticks / s_ticksPerSecond);
        }

        private static async Task<IReadOnlySet<int>> GetChildProcessIdsLinuxAsync(int pid)
        {
            var immediateChildren = await GetImmediateChildProcessIdsLinuxAsync(pid).ConfigureAwait(false);
            if (immediateChildren.Count == 0)
            {
                return immediateChildren;
            }

            var children = new HashSet<int>(immediateChildren);
            foreach (var cpid in immediateChildren)
            {
                var c = await GetChildProcessIdsLinuxAsync(cpid).ConfigureAwait(false);
                foreach (var cpid2 in c)
                {
                    children.Add(cpid2);
                }
            }

            return children;
        }

        private static async Task<IReadOnlySet<int>> GetImmediateChildProcessIdsLinuxAsync(int pid)
        {
            var result = new HashSet<int>();
            // Looking for child processes
            var stdoutBuilder = new StringBuilder();
            // FIXME: child process lookup logic is not for windows here
            await RunCommandAsync(command: $"pgrep -P {pid}", stdOutBuilder: stdoutBuilder).ConfigureAwait(false);
            var stdout = stdoutBuilder.ToString().Trim();
            if (!stdout.IsEmptyOrWhiteSpace())
            {
                var matches = Regex.Matches(stdout, @"\d+", RegexOptions.Compiled);
                if (matches?.Count > 0)
                {
                    foreach (Match m in matches)
                    {
                        var cpid = int.Parse(m.Value);
                        if (cpid != pid)
                        {
                            result.Add(cpid);
                        }
                    }
                }
            }

            return result;
        }

        public static async Task<ProcessMeasurement> MeasureAsync(
            ProcessStartInfo startInfo,
            int sampleIntervalMS = 3,
            bool forceCheckChildProcesses = false,
            CancellationToken token = default)
        {
            using var cts = CancellationTokenSource.CreateLinkedTokenSource(token);

            var m = new ProcessMeasurement();
            startInfo.UseShellExecute = false;
            using var p = new Process
            {
                StartInfo = startInfo,
            };

            using var manualResetEvent = new ManualResetEventSlim(initialState: false);
            var t = Task.Factory.StartNew(() =>
            {
                IList<Process> childProcesses = null;
                var nLoop = 0;
                manualResetEvent.Wait(cts.Token);
                var pid = p.Id;
                while (!cts.Token.IsCancellationRequested)
                {
                    try
                    {
                        var isChildProcessCpuTimeCounted = false;
                        if (!s_isWindows)
                        {
                            if (procfs.TryReadStatFile(pid, out var stat))
                            {
                                var selfTicks = stat.utime + stat.stime;
                                var childrenTicks = stat.cutime + stat.cstime;
                                m.CpuTime = TicksToTimeSpanLinux(selfTicks + childrenTicks);
                                isChildProcessCpuTimeCounted = childrenTicks > 0;
                            }
                            p.Refresh();
                        }
                        else
                        {
                            p.Refresh();
                            m.CpuTime = p.TotalProcessorTime;
                        }

                        var totalMemoryBytes = p.WorkingSet64;

                        // Look for children process after first recording
                        if (childProcesses == null && (isChildProcessCpuTimeCounted || forceCheckChildProcesses))
                        {
                            // Prevent 2nd check
                            childProcesses = new List<Process>();
                            var childrenSet = GetChildProcessIdsLinuxAsync(pid).ConfigureAwait(false).GetAwaiter().GetResult();
                            if (childrenSet?.Count > 0)
                            {
                                foreach (var cpid in childrenSet)
                                {
                                    try
                                    {
                                        childProcesses.Add(Process.GetProcessById(cpid));
                                    }
                                    catch (ArgumentException e)
                                    {
                                        Logger.Warn(e);
                                    }
                                    catch (Exception e)
                                    {
                                        Logger.Error(e);
                                    }
                                }
                            }
                        }

                        if (childProcesses?.Count > 0)
                        {
                            foreach (var cp in childProcesses)
                            {
                                try
                                {
                                    if (!cp.HasExited)
                                    {
                                        cp.Refresh();
                                        if (!isChildProcessCpuTimeCounted)
                                        {
                                            if (s_isWindows)
                                            {
                                                m.CpuTime += cp.TotalProcessorTime;
                                            }
                                            else if (procfs.TryReadStatFile(cp.Id, out var cpstat))
                                            {
                                                m.CpuTime += TicksToTimeSpanLinux(cpstat.utime + cpstat.stime);
                                            }
                                        }

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

                        if (nLoop < 50)
                        {
                        }
                        else if (nLoop < 200 || m.PeakMemoryBytes <= 0)
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
                        if (cts.Token.IsCancellationRequested || p.HasExited)
                        {
                            return;
                        }

                        Thread.Sleep(1);
                    }
                }
            }, cts.Token, TaskCreationOptions.LongRunning, TaskScheduler.Default);

            var sw = Stopwatch.StartNew();
            var ret = RunProcess(
                p,
                printOnConsole: false,
                asyncRead: true,
                stdOutBuilder: null,
                stdErrorBuilder: null,
                token,
                onStart: () => manualResetEvent.Set());

            sw.Stop();
            cts.Cancel();
            m.Elapsed = sw.Elapsed;
            await t.ConfigureAwait(false);
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

            await Task.Yield();

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
                        await Task.Delay(500).ConfigureAwait(false);
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
