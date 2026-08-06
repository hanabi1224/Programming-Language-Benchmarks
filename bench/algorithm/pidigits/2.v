module main

import os
import strconv
import math.big
import strings

const one = big.integer_from_int(1)
const two = big.integer_from_int(2)
const three = big.integer_from_int(3)
const four = big.integer_from_int(4)
const ten = big.integer_from_int(10)

fn main() {
	mut n := 27
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	digits_to_print := n
	mut digits_printed := 0
	mut k := 1
	mut n1 := four
	mut n2 := three
	mut d := one

	mut sb := strings.new_builder(digits_to_print / 10 * 24 + 32)
	for {
		// `n1 > n2` holds throughout, so floor(n1/d) and floor(n2/d) agree
		// exactly when `n1 < (floor(n2/d) + 1) * d`.
		v := n2 / d
		v_int := v.int()
		upper := (v + one) * d

		if n1 < upper {
			sb.write_u8(u8(`0` + v_int))
			digits_printed++
			digits_printed_mod_ten := digits_printed % 10
			if digits_printed_mod_ten == 0 {
				sb.write_string('\t:${digits_printed}\n')
			}

			if digits_printed >= digits_to_print {
				if digits_printed_mod_ten > 0 {
					for _ in 0 .. (10 - digits_printed_mod_ten) {
						sb.write_u8(` `)
					}
					sb.write_string('\t:${digits_printed}\n')
				}
				print(sb.str())
				return
			}

			to_minus := v * ten * d
			n1 = n1 * ten - to_minus
			n2 = n2 * ten - to_minus
		} else {
			prev_n1 := n1
			n1 = n1 * big.integer_from_int(2 * k - 1) + n2 * two
			n2 = prev_n1 * big.integer_from_int(k - 1) + n2 * big.integer_from_int(k + 2)
			d = d * big.integer_from_int(2 * k + 1)
			k++
		}
	}
}
