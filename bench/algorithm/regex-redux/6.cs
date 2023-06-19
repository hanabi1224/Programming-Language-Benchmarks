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

        var magicTask = () =>
        {
            var newseq = MagicRe1().Replace(sequences, "<4>");
            newseq = MagicRe2().Replace(newseq, "<3>");
            newseq = MagicRe3().Replace(newseq, "<2>");
            newseq = MagicRe4().Replace(newseq, "|");
            newseq = MagicRe5().Replace(newseq, "-");
            return newseq.Length;
        };

        var variant2 = "[cgt]gggtaaa|tttaccc[acg] " + Re2().Count(_sequences);
        var variant3 = "a[act]ggtaaa|tttacc[agt]t " + Re3().Count(_sequences);
        var variant7 = "agggt[cgt]aa|tt[acg]accct " + Re7().Count(_sequences);
        var variant6 = "aggg[acg]aaa|ttt[cgt]ccct " + Re6().Count(_sequences);
        var variant4 = "ag[act]gtaaa|tttac[agt]ct " + Re4().Count(_sequences);
        var variant5 = "agg[act]taaa|ttta[agt]cct " + Re5().Count(_sequences);
        var variant1 = "agggtaaa|tttaccct " + Re1().Count(_sequences);
        var variant9 = "agggtaa[cgt]|[acg]ttaccct " + Re9().Count(_sequences);
        var variant8 = "agggta[cgt]a|t[acg]taccct " + Re8().Count(_sequences);

        Console.WriteLine(variant1);
        Console.WriteLine(variant2);
        Console.WriteLine(variant3);
        Console.WriteLine(variant4);
        Console.WriteLine(variant5);
        Console.WriteLine(variant6);
        Console.WriteLine(variant7);
        Console.WriteLine(variant8);
        Console.WriteLine(variant9);
        Console.WriteLine($"\n{initialLength}\n{sequences.Length}");
        Console.WriteLine(magicTask().ToString());
    }
}
