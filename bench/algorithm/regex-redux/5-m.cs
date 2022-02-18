/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 
   Regex-Redux by Josh Goldfoot
   order variants by execution time by Anthony Lloyd
*/

using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

public static class regexredux
{
    static regexredux()
    {
        Regex.CacheSize = 1024;
    }

    static Regex regex(string re)
    {
        // Not compiled on .Net Core, hence poor benchmark results.
        return new Regex(re, RegexOptions.Compiled);
    }

    static string regexCount(string s, string r)
    {
        int c = 0;
        var m = regex(r).Match(s);
        while (m.Success) { c++; m = m.NextMatch(); }
        return r + " " + c;
    }

    public static async Task Main(string[] args)
    {
        var fileName = args.Length > 0 ? args[0] : "25000_in";
        var sequences = await File.ReadAllTextAsync(fileName).ConfigureAwait(false);
        var initialLength = sequences.Length;
        sequences = Regex.Replace(sequences, ">.*\n|\n", "");

        var magicTask = Task.Run(() =>
        {
            var newseq = regex("tHa[Nt]").Replace(sequences, "<4>");
            newseq = regex("aND|caN|Ha[DS]|WaS").Replace(newseq, "<3>");
            newseq = regex("a[NSt]|BY").Replace(newseq, "<2>");
            newseq = regex("<[^>]*>").Replace(newseq, "|");
            newseq = regex("\\|[^|][^|]*\\|").Replace(newseq, "-");
            return newseq.Length;
        });

        var variant2 = Task.Run(() => regexCount(sequences, "[cgt]gggtaaa|tttaccc[acg]"));
        var variant3 = Task.Run(() => regexCount(sequences, "a[act]ggtaaa|tttacc[agt]t"));
        var variant7 = Task.Run(() => regexCount(sequences, "agggt[cgt]aa|tt[acg]accct"));
        var variant6 = Task.Run(() => regexCount(sequences, "aggg[acg]aaa|ttt[cgt]ccct"));
        var variant4 = Task.Run(() => regexCount(sequences, "ag[act]gtaaa|tttac[agt]ct"));
        var variant5 = Task.Run(() => regexCount(sequences, "agg[act]taaa|ttta[agt]cct"));
        var variant1 = Task.Run(() => regexCount(sequences, "agggtaaa|tttaccct"));
        var variant9 = Task.Run(() => regexCount(sequences, "agggtaa[cgt]|[acg]ttaccct"));
        var variant8 = Task.Run(() => regexCount(sequences, "agggta[cgt]a|t[acg]taccct"));

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
