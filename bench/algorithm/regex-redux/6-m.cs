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
    [GeneratedRegex(@">.*\n|\n")]
    public static partial Regex ReplaceRegex();
    [GeneratedRegex(@"tHa[Nt]")]
    public static partial Regex MagicRe1();
    [GeneratedRegex(@"aND|caN|Ha[DS]|WaS")]
    public static partial Regex MagicRe2();
    [GeneratedRegex(@"a[NSt]|BY")]
    public static partial Regex MagicRe3();
    [GeneratedRegex(@"<[^>]*>")]
    public static partial Regex MagicRe4();
    [GeneratedRegex(@"\|[^|][^|]*\|")]
    public static partial Regex MagicRe5();
    [GeneratedRegex(@"[cgt]gggtaaa|tttaccc[acg]")]
    public static partial Regex Re2();
    [GeneratedRegex(@"a[act]ggtaaa|tttacc[agt]t")]
    public static partial Regex Re3();
    [GeneratedRegex(@"agggt[cgt]aa|tt[acg]accct")]
    public static partial Regex Re7();
    [GeneratedRegex(@"aggg[acg]aaa|ttt[cgt]ccct")]
    public static partial Regex Re6();
    [GeneratedRegex(@"ag[act]gtaaa|tttac[agt]ct")]
    public static partial Regex Re4();
    [GeneratedRegex(@"agg[act]taaa|ttta[agt]cct")]
    public static partial Regex Re5();
    [GeneratedRegex(@"agggtaaa|tttaccct")]
    public static partial Regex Re1();
    [GeneratedRegex(@"agggtaa[cgt]|[acg]ttaccct")]
    public static partial Regex Re9();
    [GeneratedRegex(@"agggta[cgt]a|t[acg]taccct")]
    public static partial Regex Re8();

    public static async Task Main(string[] args)
    {
        var fileName = args.Length > 0 ? args[0] : "25000_in";
        var sequences = await File.ReadAllTextAsync(fileName).ConfigureAwait(false);
        var initialLength = sequences.Length;
        sequences = ReplaceRegex().Replace(sequences, "");

        var magicTask = Task.Run(() =>
        {
            var newseq = MagicRe1().Replace(sequences, "<4>");
            newseq = MagicRe2().Replace(newseq, "<3>");
            newseq = MagicRe3().Replace(newseq, "<2>");
            newseq = MagicRe4().Replace(newseq, "|");
            newseq = MagicRe5().Replace(newseq, "-");
            return newseq.Length;
        });

        var variant2 = Task.Run(() => "[cgt]gggtaaa|tttaccc[acg] " + Re2().Count(_sequences));
        var variant3 = Task.Run(() => "a[act]ggtaaa|tttacc[agt]t " + Re3().Count(_sequences));
        var variant7 = Task.Run(() => "agggt[cgt]aa|tt[acg]accct " + Re7().Count(_sequences));
        var variant6 = Task.Run(() => "aggg[acg]aaa|ttt[cgt]ccct " + Re6().Count(_sequences));
        var variant4 = Task.Run(() => "ag[act]gtaaa|tttac[agt]ct " + Re4().Count(_sequences));
        var variant5 = Task.Run(() => "agg[act]taaa|ttta[agt]cct " + Re5().Count(_sequences));
        var variant1 = Task.Run(() => "agggtaaa|tttaccct " + Re1().Count(_sequences));
        var variant9 = Task.Run(() => "agggtaa[cgt]|[acg]ttaccct " + Re9().Count(_sequences));
        var variant8 = Task.Run(() => "agggta[cgt]a|t[acg]taccct " + Re8().Count(_sequences));

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
