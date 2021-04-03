module main

import os
import strconv
import hanabi1224.biginteger
import strings

fn main() {
	mut n := 1000
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	digits_to_print := n
	mut digits_printed := 0
	mut k := biginteger.one
	mut n1 := biginteger.four
	mut n2 := biginteger.from_int(3)
	mut d := biginteger.one
	mut u := biginteger.zero
	mut v := biginteger.zero
	mut w := biginteger.zero
	one := biginteger.one
	two := biginteger.two
	ten := biginteger.ten

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
