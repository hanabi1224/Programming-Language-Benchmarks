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
        sequences = Regex.Replace(sequences, ">.*\n|\n", "");

        var magicTask = () =>
        {
            var newseq = Regex.Replace(sequences, "tHa[Nt]", "<4>", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "aND|caN|Ha[DS]|WaS", "<3>", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "a[NSt]|BY", "<2>", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "<[^>]*>", "|", RegexOptions.Compiled);
            newseq = Regex.Replace(newseq, "\\|[^|][^|]*\\|", "-", RegexOptions.Compiled);
            return newseq.Length;
        };

        var variant2 = "[cgt]gggtaaa|tttaccc[acg] " + Regex.Count(sequences, "[cgt]gggtaaa|tttaccc[acg]", RegexOptions.Compiled);
        var variant3 = "a[act]ggtaaa|tttacc[agt]t " + Regex.Count(sequences, "a[act]ggtaaa|tttacc[agt]t", RegexOptions.Compiled);
        var variant7 = "agggt[cgt]aa|tt[acg]accct " + Regex.Count(sequences, "agggt[cgt]aa|tt[acg]accct", RegexOptions.Compiled);
        var variant6 = "aggg[acg]aaa|ttt[cgt]ccct " + Regex.Count(sequences, "aggg[acg]aaa|ttt[cgt]ccct", RegexOptions.Compiled);
        var variant4 = "ag[act]gtaaa|tttac[agt]ct " + Regex.Count(sequences, "ag[act]gtaaa|tttac[agt]ct", RegexOptions.Compiled);
        var variant5 = "agg[act]taaa|ttta[agt]cct " + Regex.Count(sequences, "agg[act]taaa|ttta[agt]cct", RegexOptions.Compiled);
        var variant1 = "agggtaaa|tttaccct " + Regex.Count(sequences, "agggtaaa|tttaccct", RegexOptions.Compiled);
        var variant9 = "agggtaa[cgt]|[acg]ttaccct " + Regex.Count(sequences, "agggtaa[cgt]|[acg]ttaccct", RegexOptions.Compiled);
        var variant8 = "agggta[cgt]a|t[acg]taccct " + Regex.Count(sequences, "agggta[cgt]a|t[acg]taccct", RegexOptions.Compiled);

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
