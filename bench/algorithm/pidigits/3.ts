/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Denis Gribov
 *    a translation of the C program contributed by Mr Ledhug
 *    modified for typescript deno by hanabi1224
 */

import { encode } from "https://deno.land/std/encoding/utf8.ts";

function write(str: string) {
    Deno.stdout.write(encode(str));
}

(function main() {
    // Int32
    let n = +Deno.args[0] || 10000,
        i = 0,
        k = 0,
        d = 0,
        k2 = 0,
        d3 = 0,
        d4 = 0;

    // BigInt
    let tmp1 = 0n, // mpz_init(tmp1)
        tmp2 = 0n, // mpz_init(tmp2)
        acc = 0n, // mpz_init_set_ui(acc, 0)
        den = 1n, // mpz_init_set_ui(den, 1)
        num = 1n; // mpz_init_set_ui(num, 1)

    const chr_0 = "0".charCodeAt(0);

    // preallocated buffer size
    // let bufsize = (10/*value of pi*/ + 2/*\t:*/ + n.toString().length/*index of slice*/ + 1/*\n*/) * (n / 10)/*line count*/;
    // croped buffer size
    // for (let i = 10, length = 10 ** (Math.log10(n) >>> 0); i < length; i *= 10) {
    //     bufsize -= i - 1;
    // }

    // let buf = Buffer.allocUnsafe(bufsize),
    //     bufoffs = 0;

    for (let i = 0; ;) {
        k++;

        //#region inline nextTerm(k)
        k2 = k * 2 + 1;
        acc += num * 2n; // mpz_addmul_ui(acc, num, 2)
        acc *= BigInt(k2); // mpz_mul_ui(acc, acc, k2)
        den *= BigInt(k2); // mpz_mul_ui(den, den, k2)
        num *= BigInt(k); // mpz_mul_ui(num, num, k)
        //#endregion inline nextTerm(k)

        if (num > acc/* mpz_cmp(num, acc) > 0 */) continue;

        //#region inline extractDigit(3);
        tmp1 = num * 3n; // mpz_mul_ui(tmp1, num, nth);
        tmp2 = tmp1 + acc; // mpz_add(tmp2, tmp1, acc);
        tmp1 = tmp2 / den; // mpz_tdiv_q(tmp1, tmp2, den);
        d3 = Number(tmp1) >>> 0; // mpz_get_ui(tmp1)
        //#region inline extractDigit(3);

        d = d3;

        //#region inline extractDigit(4);
        tmp1 = num * 4n; // mpz_mul_ui(tmp1, num, nth);
        tmp2 = tmp1 + acc; // mpz_add(tmp2, tmp1, acc);
        tmp1 = tmp2 / den; // mpz_tdiv_q(tmp1, tmp2, den);
        d4 = Number(tmp1) >>> 0; // mpz_get_ui(tmp1)
        //#region inline extractDigit(4);

        if (d !== d4) continue;

        write(String.fromCharCode(d + chr_0));

        let iMod10 = ++i % 10;
        if (iMod10 === 0) {
            write(`\t:${i}\n`);
        }

        if (i >= n) {
            if (iMod10 > 0) {
                for (let idx = 0; idx < 10 - iMod10; idx++) {
                    write(' ');
                }
                write(`\t:${i}\n`);
            }

            break;
        }

        //#region inline eliminateDigit(d)
        acc -= den * BigInt(d); // mpz_submul_ui(acc, den, d)
        acc *= 10n; // mpz_mul_ui(acc, acc, 10)
        num *= 10n; // mpz_mul_ui(num, num, 10)
        //#endregion inline eliminateDigit(d)
    }
})();
