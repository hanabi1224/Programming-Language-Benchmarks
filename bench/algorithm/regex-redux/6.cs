/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 
   Regex-Redux by Josh Goldfoot
   order variants by execution time by Anthony Lloyd
*/

using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

public static partial class regexredux
{
    const RegexOptions opt = RegexOptions.Compiled;

    static regexredux()
    {
        Regex.CacheSize = 1024;
    }

    [GeneratedRegex(@">.*\n|\n", opt)]
    public static partial Regex ReplaceRegex();

    [GeneratedRegex(@"tHa[Nt]", opt)]
    public static partial Regex MagicRe1();

    [GeneratedRegex(@"aND|caN|Ha[DS]|WaS", opt)]
    public static partial Regex MagicRe2();

    [GeneratedRegex(@"a[NSt]|BY", opt)]
    public static partial Regex MagicRe3();

    [GeneratedRegex(@"<[^>]*>", opt)]
    public static partial Regex MagicRe4();

    [GeneratedRegex(@"\|[^|][^|]*\|", opt)]
    public static partial Regex MagicRe5();

    // var variant2 = Task.Run(() => regexCount(sequences, "[cgt]gggtaaa|tttaccc[acg]"));
    [GeneratedRegex(@"[cgt]gggtaaa|tttaccc[acg]", opt)]
    public static partial Regex Re2();
    // var variant3 = Task.Run(() => regexCount(sequences, "a[act]ggtaaa|tttacc[agt]t"));
    [GeneratedRegex(@"a[act]ggtaaa|tttacc[agt]t", opt)]
    public static partial Regex Re3();
    // var variant7 = Task.Run(() => regexCount(sequences, "agggt[cgt]aa|tt[acg]accct"));
    [GeneratedRegex(@"agggt[cgt]aa|tt[acg]accct", opt)]
    public static partial Regex Re7();
    // var variant6 = Task.Run(() => regexCount(sequences, "aggg[acg]aaa|ttt[cgt]ccct"));
    [GeneratedRegex(@"aggg[acg]aaa|ttt[cgt]ccct", opt)]
    public static partial Regex Re6();
    // var variant4 = Task.Run(() => regexCount(sequences, "ag[act]gtaaa|tttac[agt]ct"));
    [GeneratedRegex(@"ag[act]gtaaa|tttac[agt]ct", opt)]
    public static partial Regex Re4();
    // var variant5 = Task.Run(() => regexCount(sequences, "agg[act]taaa|ttta[agt]cct"));
    [GeneratedRegex(@"agg[act]taaa|ttta[agt]cct", opt)]
    public static partial Regex Re5();
    // var variant1 = Task.Run(() => regexCount(sequences, "agggtaaa|tttaccct"));
    [GeneratedRegex(@"agggtaaa|tttaccct", opt)]
    public static partial Regex Re1();
    // var variant9 = Task.Run(() => regexCount(sequences, "agggtaa[cgt]|[acg]ttaccct"));
    [GeneratedRegex(@"agggtaa[cgt]|[acg]ttaccct", opt)]
    public static partial Regex Re9();
    // var variant8 = Task.Run(() => regexCount(sequences, "agggta[cgt]a|t[acg]taccct"));
    [GeneratedRegex(@"agggta[cgt]a|t[acg]taccct", opt)]
    public static partial Regex Re8();

    static string regexCount(string s, Regex r)
    {
        int c = 0;
        var m = r.Match(s);
        while (m.Success) { c++; m = m.NextMatch(); }
        return r + " " + c;
    }

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

        var variant2 = () => regexCount(sequences, Re2());
        var variant3 = () => regexCount(sequences, Re3());
        var variant7 = () => regexCount(sequences, Re7());
        var variant6 = () => regexCount(sequences, Re6());
        var variant4 = () => regexCount(sequences, Re4());
        var variant5 = () => regexCount(sequences, Re5());
        var variant1 = () => regexCount(sequences, Re1());
        var variant9 = () => regexCount(sequences, Re9());
        var variant8 = () => regexCount(sequences, Re8());

        Console.WriteLine(variant1());
        Console.WriteLine(variant2());
        Console.WriteLine(variant3());
        Console.WriteLine(variant4());
        Console.WriteLine(variant5());
        Console.WriteLine(variant6());
        Console.WriteLine(variant7());
        Console.WriteLine(variant8());
        Console.WriteLine(variant9());
        Console.WriteLine($"\n{initialLength}\n{sequences.Length}");
        Console.WriteLine(magicTask().ToString());
    }
}
