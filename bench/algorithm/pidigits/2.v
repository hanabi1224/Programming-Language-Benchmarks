module main

import os
import strconv
import math.big
import strings

const (
	zero  = big.integer_from_int(0)
	one   = big.integer_from_int(1)
	two   = big.integer_from_int(2)
	three = big.integer_from_int(3)
	four  = big.integer_from_int(4)
	ten   = big.integer_from_int(10)
)

fn main() {
	mut n := 27
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	digits_to_print := n
	mut digits_printed := 0
	mut k := one
	mut n1 := four
	mut n2 := three
	mut d := one
	mut u := zero
	mut v := zero
	mut w := zero

	mut sb := strings.new_builder(12 + n.str().len)
	unsafe {
		defer {
			sb.free()
		}
	}
	for {
		u = n1 / d
		v = n2 / d
		u_int := u.int()
		v_int := v.int()

		if u_int == v_int {
			sb.write_b(u_int.str()[0])
			digits_printed++
			digits_printed_mod_ten := digits_printed % 10
			if digits_printed_mod_ten == 0 {
				sb.write_string('\t:$digits_printed')
				println(sb.str())
				sb.go_back_to(0)
			}

			if digits_printed >= digits_to_print {
				if digits_printed_mod_ten > 0 {
					for _ in 0 .. (10 - digits_printed_mod_ten) {
						sb.write_b(` `)
					}
					sb.write_string('\t:$digits_printed')
					println(sb.str())
				}
				return
			}

			to_minus := u * ten * d
			n1 = n1 * ten - to_minus
			n2 = n2 * ten - to_minus
		} else {
			k2 := k * two
			u = n1 * (k2 - one)
			v = n2 * two
			w = n1 * (k - one)
			n1 = u + v
			u = n2 * (k + two)
			n2 = w + u
			d = d * (k2 + one)
			k += one
		}
	}
}
