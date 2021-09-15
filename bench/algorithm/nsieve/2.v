module main

import os
import strconv
import bitfield

fn main() {
	mut n := 4
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	for i := 0; i < 3; i++ {
		nsieve(10000 << (n - i))
	}
}

[direct_array_access]
fn nsieve(n int) {
	mut flags := bitfield.new(n)
	defer {
		unsafe { flags.free() }
	}
	mut count := 0
	for i := 2; i < n; i++ {
		if flags.get_bit(i) == 0 {
			count += 1
			for j := i << 1; j < n; j += i {
				flags.set_bit(j)
			}
		}
	}
	println('Primes up to ${n:8} ${count:8}')
}
