package main

import "base:runtime"
import "core:fmt"
import "core:strconv"

main :: proc() {
	args := runtime.args__
	#no_bounds_check n := strconv.parse_int(runtime.cstring_to_string(args[1])) or_else 4
	for i in 0 ..< 3 {
		shift: uint = auto_cast (n - i)
		nsieve(10000 << shift)
	}
}

nsieve :: proc(n: int) {
	flags := make([]bool, n)
	defer delete(flags)
	count := 0
	for i in 2 ..< n {
		if !flags[i] {
			count += 1
			for j := i << 1; j < n; j += i {
				flags[j] = true
			}
		}
	}
	fmt.printf("Primes up to %8s %8s\n", fmt.tprint(n), fmt.tprint(count))
}
