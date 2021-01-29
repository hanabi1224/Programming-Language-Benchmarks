using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Win32.SafeHandles;

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
        public static async Task<ProcessMeasurement> MeasureAsync(
            ProcessStartInfo startInfo,
            int sampleIntervalMS = 5,
            CancellationToken token = default)
        {
            var m = new ProcessMeasurement();
            startInfo.UseShellExecute = true;
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

                while (true)
                {
                    try
                    {
                        p.Refresh();
                        m.CpuTime = p.TotalProcessorTime;
                        if (p.WorkingSet64 > m.PeakMemoryBytes)
                        {
                            m.PeakMemoryBytes = p.WorkingSet64;
                        }

                        Thread.Sleep(sampleIntervalMS);
                        if (p.HasExited)
                        {
                            return;
                        }
                    }
                    catch (InvalidOperationException)
                    {
                        return;
                    }
                    catch (Exception e)
                    {
                        Console.Error.WriteLine(e);
                        return;
                    }
                }
            }, TaskCreationOptions.LongRunning);

            var sw = Stopwatch.StartNew();
            RunProcess(p, printOnConsole: false, null, null, token, onStart: () => started = true);
            sw.Stop();
            await t.ConfigureAwait(false);
            m.Elapsed = sw.Elapsed;
            return m;
        }

        public static async Task RunCommandAsync(
            string command,
            string workingDir = null,
            bool ensureZeroExitCode = false,
            CancellationToken token = default)
        {
            if (workingDir.IsEmptyOrWhiteSpace())
            {
                workingDir = Environment.CurrentDirectory;
            }

            ProcessStartInfo psi;
            if (Environment.OSVersion.Platform == PlatformID.Win32NT)
            {
                psi = command.ConvertToCommand();
            }
            else
            {
                psi = new ProcessStartInfo
                {
                    FileName = "sh",
                    Arguments = $"-c \"{command}\"",
                };
            }

            psi.WorkingDirectory = workingDir;
            var ret = RunProcess(psi, useShellExecute: false, printOnConsole: true, stdErrorBuilder: null, stdOutBuilder: null, token: token);
            if (ensureZeroExitCode && ret != 0)
            {
                throw new InvalidOperationException($"[Non zero exit code {ret}] {command}");
            }
        }

        public static int RunProcess(
                ProcessStartInfo startInfo,
                bool printOnConsole,
                out string stdOut,
                out string stdError,
                CancellationToken token)
        {
            var stdOutBuilder = new StringBuilder();
            var stdErrorBuilder = new StringBuilder();

            var ret = RunProcess(
                startInfo: startInfo,
                printOnConsole: printOnConsole,
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
            CancellationToken token)
        {
            return RunProcess(
                startInfo: startInfo,
                useShellExecute: false,
                printOnConsole: printOnConsole,
                stdOutBuilder: stdOutBuilder,
                stdErrorBuilder: stdErrorBuilder,
                token: token);
        }

        public static int RunProcess(
            ProcessStartInfo startInfo,
            bool useShellExecute,
            bool printOnConsole,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            CancellationToken token)
        {
            startInfo.UseShellExecute = useShellExecute;
            startInfo.RedirectStandardOutput = !useShellExecute;
            startInfo.RedirectStandardError = !useShellExecute;

            var p = new Process
            {
                StartInfo = startInfo,
            };

            return RunProcess(p: p, printOnConsole: printOnConsole, stdOutBuilder: stdOutBuilder, stdErrorBuilder: stdErrorBuilder, token: token);
        }

        public static int RunProcess(
            Process p,
            bool printOnConsole,
            StringBuilder stdOutBuilder,
            StringBuilder stdErrorBuilder,
            CancellationToken token,
            Action onStart = null)
        {
            using (p)
            {
                Console.WriteLine($"Executing command: {p.StartInfo.FileName} {p.StartInfo.Arguments}");
                var useShellExecute = p.StartInfo.UseShellExecute;
                if (p.StartInfo.RedirectStandardOutput)
                {
                    p.StartInfo.StandardOutputEncoding = Encoding.UTF8;
                    p.OutputDataReceived += (object sender, DataReceivedEventArgs e) =>
                    {
                        stdOutBuilder?.AppendLine(e.Data);
                        if (printOnConsole)
                        {
                            Console.WriteLine(e.Data);
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
                            Console.Error.WriteLine(e.Data);
                        }
                    };
                }

                p.Start();
                onStart?.Invoke();
                if (p.StartInfo.RedirectStandardOutput)
                {
                    p.BeginOutputReadLine();
                }
                if (p.StartInfo.RedirectStandardError)
                {
                    p.BeginErrorReadLine();
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
                        var outRm = p.StandardOutput.ReadToEnd();
                        if (!outRm.IsEmptyOrWhiteSpace())
                        {
                            stdOutBuilder?.Append(outRm);
                        }
                    }
                    if (p.StartInfo.RedirectStandardError)
                    {
                        var errRm = p.StandardError.ReadToEnd();
                        if (!errRm.IsEmptyOrWhiteSpace())
                        {
                            stdErrorBuilder?.Append(errRm);
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
