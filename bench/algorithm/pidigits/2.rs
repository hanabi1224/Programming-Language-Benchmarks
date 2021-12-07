use ibig::{ibig, IBig};
use std::io::{self, prelude::*, BufWriter};

fn main() -> anyhow::Result<()> {
    let digits_to_print = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(27);

    let one: IBig = ibig!(1);
    let two: IBig = ibig!(2);
    let ten: IBig = ibig!(10);

    let mut digits_printed = 0;
    let mut k = ibig!(1);
    let mut n1 = ibig!(4);
    let mut n2 = ibig!(3);
    let mut d = ibig!(1);
    let mut u: IBig;
    let mut v: IBig;
    let mut w: IBig;

    let mut stdout = BufWriter::new(io::stdout());
    loop {
        u = &n1 / &d;
        v = &n2 / &d;
        if u == v {
            stdout.write_fmt(format_args!("{}", u))?;
            digits_printed += 1;
            let digits_printed_mod_ten = &digits_printed % 10;
            if digits_printed_mod_ten == 0 {
                stdout.write_fmt(format_args!("\t:{}\n", digits_printed))?;
            }

            if digits_printed >= digits_to_print {
                if digits_printed_mod_ten > 0 {
                    for _ in 0..(10 - digits_printed_mod_ten) {
                        stdout.write_all(b" ")?;
                    }
                    stdout.write_fmt(format_args!("\t:{}\n", digits_printed))?;
                }

                return Ok(());
            }

            let to_minus = &u * &ten * &d;
            n1 = &n1 * &ten - &to_minus;
            n2 = &n2 * &ten - &to_minus;
        } else {
            let k2 = &k * &two;
            u = &n1 * (&k2 - &one);
            v = &n2 * &two;
            w = &n1 * (&k - &one);
            n1 = &u + &v;
            u = &n2 * (&k + &two);
            n2 = &w + &u;
            d = &d * (&k2 + &one);
            k = &k + &one;
        }
    }
}
