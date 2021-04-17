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
using MathNet.Numerics.Statistics;
using NLog;
using static Interop;

namespace BenchTool
{
    public static class ProcessMeasurementExtensions
    {
        public static ProcessMeasurement GetStats(this ICollection<ProcessMeasurement> array)
        {
            if (array.Count == 0)
            {
                return new ProcessMeasurement();
            }
            else if (array.Count == 1)
            {
                return array.Single();
            }

            var positiveCpuTimeRecords = array.Where(i => i.CpuTime.TotalMilliseconds > 0).ToList();
            var avgCpuTimeUser = TimeSpan.FromMilliseconds(positiveCpuTimeRecords.Count > 0 ? positiveCpuTimeRecords.Average(i => i.CpuTimeUser.TotalMilliseconds) : 0);
            var avgCpuTimeKernel = TimeSpan.FromMilliseconds(positiveCpuTimeRecords.Count > 0 ? positiveCpuTimeRecords.Average(i => i.CpuTimeKernel.TotalMilliseconds) : 0);

            var positivePeakMemoryBytes = array.Select(i => i.PeakMemoryBytes).Where(i => i > 0).ToList();
            var avgPeakMemoryBytes = positivePeakMemoryBytes.Count > 0 ? positivePeakMemoryBytes.Average() : 0;

            return new ProcessMeasurement
            {
                Elapsed = TimeSpan.FromMilliseconds(array.Average(i => i.Elapsed.TotalMilliseconds)),
                ElapsedStdDevMS = Statistics.StandardDeviation(array.Select(i => i.Elapsed.TotalMilliseconds)),
                CpuTimeKernel = avgCpuTimeKernel,
                CpuTimeUser = avgCpuTimeUser,
                PeakMemoryBytes = (long)Math.Round(avgPeakMemoryBytes),
            };
        }
    }

    public class ProcessMeasurement
    {
        public TimeSpan Elapsed { get; set; }

        public double ElapsedStdDevMS { get; set; }

        public TimeSpan CpuTime => CpuTimeUser + CpuTimeKernel;

        public TimeSpan CpuTimeUser { get; set; }

        public TimeSpan CpuTimeKernel { get; set; }

        public long PeakMemoryBytes { get; set; }

        public override string ToString()
        {
            return $"[{Environment.ProcessorCount} cores]time: {Elapsed.TotalMilliseconds}ms, stddev: {ElapsedStdDevMS}ms, cpu-time: {CpuTime.TotalMilliseconds}ms, cpu-time-user: {CpuTimeUser.TotalMilliseconds}ms, cpu-time-kernel: {CpuTimeKernel.TotalMilliseconds}ms, peak-mem: {PeakMemoryBytes / 1024}KB";
        }
    }

    public static class ProcessUtils
    {
        private static readonly bool s_isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows);
        private static readonly bool s_isLinux = RuntimeInformation.IsOSPlatform(OSPlatform.Linux);
        private static readonly bool s_isOsx = RuntimeInformation.IsOSPlatform(OSPlatform.OSX);

        private static Logger Logger { get; } = LogManager.GetCurrentClassLogger();

        private static long s_ticksPerSecond;

        static ProcessUtils()
        {
            if (s_isLinux)
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
                        long totalMemoryBytes = 0;
                        if (s_isLinux)
                        {
                            if (procfs.TryReadStatFile(pid, out var stat))
                            {
                                m.CpuTimeUser = TicksToTimeSpanLinux(stat.utime + stat.cutime);
                                m.CpuTimeKernel = TicksToTimeSpanLinux(stat.stime + stat.cstime);
                                isChildProcessCpuTimeCounted = stat.cutime > 0 || stat.cstime > 0;
                                if (procfs.TryReadStatusFile(pid, out var status))
                                {
                                    totalMemoryBytes = (long)status.VmRSS;
                                }
                            }
                            p.Refresh();
                        }
                        else
                        {
                            p.Refresh();
                            m.CpuTimeUser = p.UserProcessorTime;
                            m.CpuTimeKernel = p.PrivilegedProcessorTime;
                            totalMemoryBytes = p.WorkingSet64;
                        }

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
                                        if (!s_isLinux)
                                        {
                                            cp.Refresh();
                                            m.CpuTimeUser += cp.UserProcessorTime;
                                            m.CpuTimeKernel += cp.PrivilegedProcessorTime;
                                            totalMemoryBytes += cp.WorkingSet64;
                                        }
                                        else if (procfs.TryReadStatFile(cp.Id, out var cpstat))
                                        {
                                            if (!isChildProcessCpuTimeCounted)
                                            {
                                                m.CpuTimeUser += TicksToTimeSpanLinux(cpstat.utime);
                                                m.CpuTimeKernel += TicksToTimeSpanLinux(cpstat.stime);
                                            }
                                            if (procfs.TryReadStatusFile(cp.Id, out var cpstatus))
                                            {
                                                totalMemoryBytes += (long)cpstatus.VmRSS;
                                            }
                                        }
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
            var ret = await RunProcessAsync(
                p,
                printOnConsole: false,
                asyncRead: true,
                stdOutBuilder: null,
                stdErrorBuilder: null,
                token,
                onStart: () => manualResetEvent.Set()).ConfigureAwait(false);

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

            var psi = command.ConvertToCommand();
            psi.WorkingDirectory = workingDir;

            var ret = await RunProcessAsync(
                psi,
                useShellExecute: false,
                printOnConsole: true,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token).ConfigureAwait(false);

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

            var ret = RunProcessAsync(
                startInfo: startInfo,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token).ConfigureAwait(false).GetAwaiter().GetResult();

            stdOut = stdOutBuilder.ToString();
            stdError = stdErrorBuilder.ToString();

            return ret;
        }

        public static Task<int> RunProcessAsync(
            ProcessStartInfo startInfo,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            bool printOnConsole,
            bool asyncRead,
            CancellationToken token)
        {
            return RunProcessAsync(
                startInfo: startInfo,
                useShellExecute: false,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token);
        }

        public static Task<int> RunProcessAsync(
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

            return RunProcessAsync(
                p: p,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token);
        }

        public static async Task<int> RunProcessAsync(
            Process p,
            bool printOnConsole,
            bool asyncRead,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            CancellationToken token,
            Action onStart = null)
        {
            await Task.Yield();
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
                    p.OutputDataReceived += async (object sender, DataReceivedEventArgs e) =>
                    {
                        stdOutBuilder?.AppendLine(e.Data);
                        if (printOnConsole)
                        {
                            if (e.Data.IsEmptyOrWhiteSpace())
                            {
                                await Console.Out.WriteLineAsync(e.Data).ConfigureAwait(false);
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
                    p.ErrorDataReceived += async (object sender, DataReceivedEventArgs e) =>
                    {
                        stdErrorBuilder?.AppendLine(e.Data);
                        if (printOnConsole)
                        {
                            if (e.Data.IsEmptyOrWhiteSpace())
                            {
                                await Console.Error.WriteLineAsync(e.Data).ConfigureAwait(false);
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

                try
                {
                    // Avoid deadlock in sync mode
                    // https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.process.standardoutput?view=net-5.0
                    // To avoid deadlocks, use an asynchronous read operation on at least one of the streams.  
                    if (p.StartInfo.RedirectStandardError)
                    {
                        p.BeginErrorReadLine();
                    }

                    if (asyncRead && p.StartInfo.RedirectStandardOutput)
                    {
                        p.BeginOutputReadLine();
                    }
                }
                catch (Exception e)
                {
                    Logger.Error(e.Message);
                }

                try
                {
                    if (!asyncRead && p.StartInfo.RedirectStandardOutput)
                    {
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

                    await p.WaitForExitAsync(token).ConfigureAwait(false);
                    return p.ExitCode;
                }
                catch (Exception e)
                {
                    Logger.Error(e);
                    return -1;
                }
            }
        }
    }
}
