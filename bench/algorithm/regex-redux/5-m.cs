/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 
   Regex-Redux by Josh Goldfoot
   order variants by execution time by Anthony Lloyd
*/

using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

public static partial class RegexRedux
{
    public static async Task Main(string[] args)
    {
        var fileName = args.Length > 0 ? args[0] : "25000_in";
        var sequences = await File.ReadAllTextAsync(fileName).ConfigureAwait(false);
        var initialLength = sequences.Length;
        sequences = Regex.Replace(sequences, ">.*\n|\n", "", RegexOptions.Compiled);

        var magicTask = Task.Run(() =>
        {
            var newseq = Regex.Replace(sequences, "tHa[Nt]", "<4>", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "aND|caN|Ha[DS]|WaS", "<3>", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "a[NSt]|BY", "<2>", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "<[^>]*>", "|", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "\\|[^|][^|]*\\|", "-", RegexOptions.Compiled);
            return newseq.Length;
        });

        var variant2 = Task.Run(() => "[cgt]gggtaaa|tttaccc[acg] " + Regex.Count(sequences, "[cgt]gggtaaa|tttaccc[acg]", RegexOptions.Compiled));
        var variant3 = Task.Run(() => "a[act]ggtaaa|tttacc[agt]t " + Regex.Count(sequences, "a[act]ggtaaa|tttacc[agt]t", RegexOptions.Compiled));
        var variant7 = Task.Run(() => "agggt[cgt]aa|tt[acg]accct " + Regex.Count(sequences, "agggt[cgt]aa|tt[acg]accct", RegexOptions.Compiled));
        var variant6 = Task.Run(() => "aggg[acg]aaa|ttt[cgt]ccct " + Regex.Count(sequences, "aggg[acg]aaa|ttt[cgt]ccct", RegexOptions.Compiled));
        var variant4 = Task.Run(() => "ag[act]gtaaa|tttac[agt]ct " + Regex.Count(sequences, "ag[act]gtaaa|tttac[agt]ct", RegexOptions.Compiled));
        var variant5 = Task.Run(() => "agg[act]taaa|ttta[agt]cct " + Regex.Count(sequences, "agg[act]taaa|ttta[agt]cct", RegexOptions.Compiled));
        var variant1 = Task.Run(() => "agggtaaa|tttaccct " + Regex.Count(sequences, "agggtaaa|tttaccct", RegexOptions.Compiled));
        var variant9 = Task.Run(() => "agggtaa[cgt]|[acg]ttaccct " + Regex.Count(sequences, "agggtaa[cgt]|[acg]ttaccct", RegexOptions.Compiled));
        var variant8 = Task.Run(() => "agggta[cgt]a|t[acg]taccct " + Regex.Count(sequences, "agggta[cgt]a|t[acg]taccct", RegexOptions.Compiled));

        Task.WaitAll(variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, variant9);

        await Console.Out.WriteLineAsync(await variant1.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant2.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant3.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant4.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant5.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant6.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant7.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant8.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync(await variant9.ConfigureAwait(false)).ConfigureAwait(false);
        await Console.Out.WriteLineAsync("\n" + initialLength + "\n" + sequences.Length).ConfigureAwait(false);
        await Console.Out.WriteLineAsync((await magicTask.ConfigureAwait(false)).ToString()).ConfigureAwait(false);
    }
}
