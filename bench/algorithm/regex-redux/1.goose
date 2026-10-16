import std;
import os;
import regex;

let VARIANTS = [
    "agggtaaa|tttaccct",
    "[cgt]gggtaaa|tttaccc[acg]",
    "a[act]ggtaaa|tttacc[agt]t",
    "ag[act]gtaaa|tttac[agt]ct",
    "agg[act]taaa|ttta[agt]cct",
    "aggg[acg]aaa|ttt[cgt]ccct",
    "agggt[cgt]aa|tt[acg]accct",
    "agggta[cgt]a|t[acg]taccct",
    "agggtaa[cgt]|[acg]ttaccct",
];

let SUBST_PATTERNS = ["tHa[Nt]", "aND|caN|Ha[DS]|WaS", "a[NSt]|BY", "<[^>]*>", "\\|[^|][^|]*\\|"];
let SUBST_WITH = ["<4>", "<3>", "<2>", "|", "-"];

// text with every match of re replaced by `with`, appended to out.
fn replace_all(out: u8[>..]&, re: regex::Regex&, text: u8[:], with: u8[:]) {
    var last = 0;
    regex::each_match(re, text) { s, e =>
        out.append(text[last..s]);
        out.append(with);
        last = e;
    };
    out.append(text[last..]);
}

fn main() {
    let argv = args();
    var input: u8[>..] = [];
    if !read_file(if argv.len > 1 { argv[1] } else { "25000_in" }, input) { abort("cannot read the input"); }

    var seq: u8[>..] = [];
    let strip = regex::compile(">[^\n]*\n|\n");
    replace_all(seq, strip, input, "");

    for v in VARIANTS {
        let re = regex::compile(v);
        print(v, " ", regex::count(re, seq));
    }

    // The substitutions go back and forth between two buffers, ending in a.
    var a: u8[>..] = [];
    var b: u8[>..] = [];
    for p, i in SUBST_PATTERNS {
        let re = regex::compile(p);
        if i == 0 {
            replace_all(a, re, seq, SUBST_WITH[i]);
        } else if i % 2 == 1 {
            b.clear();
            replace_all(b, re, a, SUBST_WITH[i]);
        } else {
            a.clear();
            replace_all(a, re, b, SUBST_WITH[i]);
        }
    }
    print("\n", input.len, "\n", seq.len, "\n", a.len);
}
