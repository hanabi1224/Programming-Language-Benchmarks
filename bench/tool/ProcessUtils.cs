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
        public static ProcessMeasurement GetAverageStats(this ICollection<ProcessMeasurement> array)
        {
            if (array.Count == 0)
            {
                return new ProcessMeasurement();
            }
            else if (array.Count == 1)
            {
                return array.Single();
            }

            List<ProcessMeasurement> positiveCpuTimeRecords = array.Where(i => i.CpuTime.TotalMilliseconds > 0).ToList();
            TimeSpan avgCpuTimeUser = TimeSpan.FromMilliseconds(positiveCpuTimeRecords.Count > 0 ? positiveCpuTimeRecords.Average(i => i.CpuTimeUser.TotalMilliseconds) : 0);
            TimeSpan avgCpuTimeKernel = TimeSpan.FromMilliseconds(positiveCpuTimeRecords.Count > 0 ? positiveCpuTimeRecords.Average(i => i.CpuTimeKernel.TotalMilliseconds) : 0);

            double maxPeakMemoryBytes = array.Select(i => i.PeakMemoryBytes).Max();

            return new ProcessMeasurement
            {
                Elapsed = TimeSpan.FromMilliseconds(array.Average(i => i.Elapsed.TotalMilliseconds)),
                ElapsedStdDevMS = Statistics.StandardDeviation(array.Select(i => i.Elapsed.TotalMilliseconds)),
                CpuTimeKernel = avgCpuTimeKernel,
                CpuTimeUser = avgCpuTimeUser,
                PeakMemoryBytes = (long)Math.Round(maxPeakMemoryBytes),
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

        private static readonly long s_ticksPerSecond;

        static ProcessUtils()
        {
            if (s_isLinux)
            {
                // Look up the number of ticks per second in the system's configuration,
                // then use that to convert to a TimeSpan
                long ticksPerSecond = Interop.Sys.SysConf(Interop.Sys.SysConfName._SC_CLK_TCK);
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
            IReadOnlySet<int> immediateChildren = await GetImmediateChildProcessIdsLinuxAsync(pid).ConfigureAwait(false);
            if (immediateChildren.Count == 0)
            {
                return immediateChildren;
            }

            HashSet<int> children = new HashSet<int>(immediateChildren);
            foreach (int cpid in immediateChildren)
            {
                IReadOnlySet<int> c = await GetChildProcessIdsLinuxAsync(cpid).ConfigureAwait(false);
                foreach (int cpid2 in c)
                {
                    children.Add(cpid2);
                }
            }

            return children;
        }

        private static async Task<IReadOnlySet<int>> GetImmediateChildProcessIdsLinuxAsync(int pid)
        {
            HashSet<int> result = new HashSet<int>();
            // Looking for child processes
            StringBuilder stdoutBuilder = new StringBuilder();
            // FIXME: child process lookup logic is not for windows here
            await RunCommandAsync(command: $"pgrep -P {pid}", stdOutBuilder: stdoutBuilder).ConfigureAwait(false);
            string stdout = stdoutBuilder.ToString().Trim();
            if (!stdout.IsEmptyOrWhiteSpace())
            {
                MatchCollection matches = Regex.Matches(stdout, @"\d+", RegexOptions.Compiled);
                if (matches?.Count > 0)
                {
                    foreach (Match m in matches)
                    {
                        int cpid = int.Parse(m.Value);
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
            double timeoutSeconds = 0.0,
            CancellationToken token = default)
        {
            using CancellationTokenSource cts = CancellationTokenSource.CreateLinkedTokenSource(token);
            if (timeoutSeconds > 0)
            {
                cts.CancelAfter(TimeSpan.FromSeconds(timeoutSeconds));
            }

            ProcessMeasurement m = new ProcessMeasurement();
            startInfo.UseShellExecute = false;
            using Process p = new Process
            {
                StartInfo = startInfo,
            };

            using ManualResetEventSlim manualResetEvent = new ManualResetEventSlim(initialState: false);
            Task t = Task.Factory.StartNew(() =>
            {
                IList<Process> childProcesses = null;
                int nLoop = 0;
                manualResetEvent.Wait(cts.Token);
                int pid = p.Id;
                while (!cts.Token.IsCancellationRequested)
                {
                    try
                    {
                        bool isChildProcessCpuTimeCounted = false;
                        long totalMemoryBytes = 0;
                        if (s_isLinux)
                        {
                            if (procfs.TryReadStatFile(pid, out procfs.ParsedStat stat))
                            {
                                m.CpuTimeUser = TicksToTimeSpanLinux(stat.utime + stat.cutime);
                                m.CpuTimeKernel = TicksToTimeSpanLinux(stat.stime + stat.cstime);
                                isChildProcessCpuTimeCounted = stat.cutime > 0 || stat.cstime > 0;
                                if (procfs.TryReadStatusFile(pid, out procfs.ParsedStatus status))
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
                            IReadOnlySet<int> childrenSet = GetChildProcessIdsLinuxAsync(pid).ConfigureAwait(false).GetAwaiter().GetResult();
                            if (childrenSet?.Count > 0)
                            {
                                foreach (int cpid in childrenSet)
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
                            foreach (Process cp in childProcesses)
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
                                        else if (procfs.TryReadStatFile(cp.Id, out procfs.ParsedStat cpstat))
                                        {
                                            if (!isChildProcessCpuTimeCounted)
                                            {
                                                m.CpuTimeUser += TicksToTimeSpanLinux(cpstat.utime);
                                                m.CpuTimeKernel += TicksToTimeSpanLinux(cpstat.stime);
                                            }
                                            if (procfs.TryReadStatusFile(cp.Id, out procfs.ParsedStatus cpstatus))
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

            Stopwatch sw = Stopwatch.StartNew();
            int ret = await RunProcessAsync(
                p,
                printOnConsole: false,
                asyncRead: true,
                stdOutBuilder: null,
                stdErrorBuilder: null,
                env: null,
                cts.Token,
                onStart: () => manualResetEvent.Set()).ConfigureAwait(false);
            sw.Stop();
            cts.Cancel();
            m.Elapsed = sw.Elapsed;
            await t.ConfigureAwait(false);
            return ret < 0 ? null : m;
        }

        public static async Task RunCommandsAsync(
            IEnumerable<string> commands,
            string workingDir = null,
            bool asyncRead = false,
            bool ensureZeroExitCode = false,
            IDictionary<string, string> env = null,
            CancellationToken token = default
            )
        {
            if (commands == null)
            {
                return;
            }

            foreach (string command in commands)
            {
                await RunCommandAsync(
                    command: command,
                    workingDir: workingDir,
                    asyncRead: asyncRead,
                    ensureZeroExitCode: ensureZeroExitCode,
                    env: env,
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
            IDictionary<string, string> env = null,
            CancellationToken token = default)
        {
            if (workingDir.IsEmptyOrWhiteSpace())
            {
                workingDir = Environment.CurrentDirectory;
            }

            ProcessStartInfo psi = command.ConvertToCommand();
            psi.WorkingDirectory = workingDir;

            int ret = await RunProcessAsync(
                psi,
                useShellExecute: false,
                printOnConsole: true,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                env: env,
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
                IDictionary<string, string> env,
                CancellationToken token)
        {
            StringBuilder stdOutBuilder = new StringBuilder();
            StringBuilder stdErrorBuilder = new StringBuilder();

            int ret = RunProcessAsync(
                startInfo: startInfo,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                env: env,
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
            IDictionary<string, string> env,
            CancellationToken token)
        {
            return RunProcessAsync(
                startInfo: startInfo,
                useShellExecute: false,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                env: env,
                token: token);
        }

        public static Task<int> RunProcessAsync(
            ProcessStartInfo startInfo,
            bool useShellExecute,
            bool printOnConsole,
            bool asyncRead,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            IDictionary<string, string> env,
            CancellationToken token)
        {
            startInfo.UseShellExecute = useShellExecute;

            Process p = new Process
            {
                StartInfo = startInfo,
            };

            return RunProcessAsync(
                p: p,
                printOnConsole: printOnConsole,
                asyncRead: asyncRead,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                env: env,
                token: token);
        }

        public static async Task<int> RunProcessAsync(
            Process p,
            bool printOnConsole,
            bool asyncRead,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            IDictionary<string, string> env,
            CancellationToken token,
            Action onStart = null)
        {
            await Task.Yield();
            using (p)
            {
                bool useShellExecute = p.StartInfo.UseShellExecute;
                p.StartInfo.RedirectStandardOutput = !useShellExecute;
                p.StartInfo.RedirectStandardError = !useShellExecute;
                if (env?.Count > 0)
                {
                    foreach (KeyValuePair<string, string> pair in env)
                    {
                        p.StartInfo.Environment[pair.Key] = pair.Value;
                    }
                }

                string prefix = $"Command[shell:{useShellExecute},print:{printOnConsole},async:{asyncRead}]:";
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
                else
                {
                    p.StartInfo.StandardOutputEncoding = null;
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
                else
                {
                    p.StartInfo.StandardErrorEncoding = null;
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
                        string outRm = p.StandardOutput.ReadToEnd();
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
                    if (!(e is TaskCanceledException))
                    {
                        Logger.Error(e);
                    }
                    try
                    {
                        if (!p.HasExited)
                        {
                            Logger.Warn("Killing dangling processes");
                            p.Kill(true);
                        }
                    }
                    catch (Exception innerExc)
                    {
                        Logger.Error(innerExc);
                    }
                    return -1;
                }
            }
        }
    }
}
