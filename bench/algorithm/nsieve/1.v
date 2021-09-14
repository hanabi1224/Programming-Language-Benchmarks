module main

import os
import strconv

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
	mut flags := []bool{len: n, init: true}
	mut count := 0
	for i := 2; i < n; i++ {
		if flags[i] {
			count += 1
			for j := i << 1; j < n; j += i {
				flags[j] = false
			}
		}
	}
	println('Primes up to ${n:8} ${count:8}')
}
