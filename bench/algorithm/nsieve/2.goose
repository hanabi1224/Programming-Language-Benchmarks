import std;
import os;

// The flags are bits of u32 words, a set bit marking a composite, as in the
// second Rust program.

fn nsieve(n: i64) {
    var flags: u32[>..] = [];
    flags.resize((n + 31) >> 5, 0);
    var count = 0;
    for i in 2..n {
        if (flags[i >> 5] >> i) & 1 == 0 {
            count++;
            var j = i << 1;
            while j < n {
                flags[j >> 5] |= (1 as u32) << j;
                j += i;
            }
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
