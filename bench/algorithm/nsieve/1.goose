import std;
import os;

// The flags are one byte each, as in the C, C++ and first Rust programs.

fn nsieve(n: i64) {
    var flags: bool[>..] = [];
    flags.resize(n, true);
    var count = 0;
    for i in 2..n {
        if flags[i] {
            count++;
            var j = i << 1;
            while j < n { flags[j] = false; j += i; }
        }
    }
    var line: u8[>..] = [];
    line.append("Primes up to ");
    format_int(line, n, 10, 8, ' ');
    line.push(' ');
    format_int(line, count, 10, 8, ' ');
    print(line);
}

fn main() {
    let a = args();
    var n = 4;
    if a.len > 1 { let v, ok = parse_int(a[1]); if ok { n = v; } }
    for i in 3 { nsieve(10000 << (n - i)); }
}
