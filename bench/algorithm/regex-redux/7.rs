// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Tom Kaitchuck
// removed parallelization

use std::io::{self, BufWriter, Read, Write};
use std::mem;

struct Regex {
    string: &'static str,
    regex: ::regex::bytes::Regex,
    replacement: Option<&'static [u8]>,
}

impl Regex {
    fn new(string: &'static str, replacement: Option<&'static [u8]>) -> Regex {
        Regex {
            string: string,
            regex: ::regex::bytes::Regex::new(string).unwrap(),
            replacement,
        }
    }

    fn replace_all<'t>(&self, text: &'t [u8], out: &mut Vec<u8>) {
        let mut last_match = 0;
        for m in self.regex.find_iter(text) {
            out.extend_from_slice(&text[last_match..m.start()]);
            if let Some(rep) = self.replacement {
                out.extend_from_slice(rep);
            }
            last_match = m.end();
        }
        out.extend_from_slice(&text[last_match..]);
    }
}

fn count_reverse_complements(sequence: &Vec<u8>) -> Vec<String> {
    // Search for occurrences of the following patterns:
    lazy_static::lazy_static! {
        static ref VARIANTS: Vec<Regex> = vec![
            Regex::new("agggtaaa|tttaccct", None),
            Regex::new("[cgt]gggtaaa|tttaccc[acg]", None),
            Regex::new("a[act]ggtaaa|tttacc[agt]t", None),
            Regex::new("ag[act]gtaaa|tttac[agt]ct", None),
            Regex::new("agg[act]taaa|ttta[agt]cct", None),
            Regex::new("aggg[acg]aaa|ttt[cgt]ccct", None),
            Regex::new("agggt[cgt]aa|tt[acg]accct", None),
            Regex::new("agggta[cgt]a|t[acg]taccct", None),
            Regex::new("agggtaa[cgt]|[acg]ttaccct", None),
        ];
    }
    VARIANTS
        .iter()
        .map(|variant| {
            format!(
                "{} {}",
                variant.string,
                variant.regex.find_iter(sequence).count()
            )
        })
        .collect()
}

fn find_replaced_sequence_length(sequence: &Vec<u8>, scratch_buff: &mut Vec<u8>) -> usize {
    // Replace the following patterns, one at a time:
    lazy_static::lazy_static! {
        static ref SUBSTS: Vec<Regex> = vec![
            Regex::new("tHa[Nt]", Some(b"<4>")),
            Regex::new("aND|caN|Ha[DS]|WaS", Some(b"<3>")),
            Regex::new("a[NSt]|BY", Some(b"<2>")),
            Regex::new("<[^>]*>", Some(b"|")),
            Regex::new("\\|[^|][^|]*\\|", Some(b"-")),
        ];
    }

    let mut first_run = true;
    let mut current = Vec::with_capacity(scratch_buff.len() + sequence.len());
    let next = scratch_buff;
    // Perform the replacements in sequence:
    for re in SUBSTS.iter() {
        if first_run {
            re.replace_all(sequence, next);
            first_run = false;
        } else {
            re.replace_all(&current, next);
        }
        mem::swap(&mut current, next);
        if next.len() > 0 {
            next.clear();
        }
    }
    current.len()
}

fn main() -> anyhow::Result<()> {
    let file_name = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .unwrap_or("25000_in".into());

    let mut input = {
        let mut v = Vec::with_capacity(51 * (1 << 20));
        let mut file = std::fs::File::open(file_name).unwrap();
        file.read_to_end(&mut v).unwrap();
        v
    };
    let input_len = input.len();
    let mut sequence: Vec<u8> = Vec::with_capacity(input.len());
    Regex::new(">[^\n]*\n|\n", None).replace_all(&input, &mut sequence);
    let sequence_len = sequence.len();
    input.clear();
    let (result, counts) = rayon::join(
        || find_replaced_sequence_length(&sequence, &mut input),
        || count_reverse_complements(&sequence),
    );
    let mut stdout = BufWriter::new(io::stdout());
    for variant in counts {
        stdout.write_fmt(format_args!("{}\n", variant))?;
    }
    stdout.write_fmt(format_args!(
        "\n{}\n{}\n{:?}\n",
        input_len, sequence_len, result
    ))?;
    Ok(())
}
