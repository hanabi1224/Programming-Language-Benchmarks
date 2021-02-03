use num_bigint::BigInt;

fn main() {
    let digits_to_print = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(27);

    let mut digits_printed = 0;
    let mut k = BigInt::from(1);
    let mut n1 = BigInt::from(4);
    let mut n2 = BigInt::from(3);
    let mut d = BigInt::from(1);
    let mut u: BigInt;
    let mut v: BigInt;
    let mut w: BigInt;

    loop {
        u = &n1 / &d;
        v = &n2 / &d;
        if u == v {
            print!("{}", u);
            digits_printed += 1;
            let digits_printed_mod_ten = &digits_printed % 10;
            if digits_printed_mod_ten == 0 {
                println!("\t:{}", digits_printed);
            }

            if digits_printed >= digits_to_print {
                if digits_printed_mod_ten > 0 {
                    for _ in 0..(10 - digits_printed_mod_ten) {
                        print!(" ")
                    }
                    println!("\t:{}", digits_printed);
                }

                return;
            }

            let to_minus = &u * 10 * &d; //u.mul(i10).mul(&d);
            n1 = &n1 * 10 - &to_minus;
            n2 = &n2 * 10 - &to_minus;
        } else {
            let k2 = &k * 2;
            u = &n1 * (&k2 - 1);
            v = &n2 * 2;
            w = &n1 * (&k - 1);
            n1 = &u + &v;
            u = &n2 * (&k + 2);
            n2 = &w + &u;
            d = &d * (&k2 + 1);
            k = &k + 1;
        }
    }
}
