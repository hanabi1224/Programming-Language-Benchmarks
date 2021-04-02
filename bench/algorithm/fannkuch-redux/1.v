module main

import os
import strconv

fn main() {
	mut n := 10
	if os.args.len == 2 {
		n = strconv.atoi(os.args[1]) or { 10 }
	}

	sum, flips := fannkuchredux(n)

	println('$sum\nPfannkuchen($n) = $flips')
}

[direct_array_access]
fn fannkuchredux(n int) (int, int) {
	mut perm1 := [32]int{}
	for i in 0 .. perm1.len {
		perm1[i] = i
	}
	mut perm := [32]int{}
	mut count := [32]int{}
	mut checksum := 0
	mut perm_count := 0
	mut n_max_flips := 0
	mut r := n
	for true {
		for r > 1 {
			count[r - 1] = r
			r -= 1
		}

		for i in 0 .. n {
			perm[i] = perm1[i]
		}

		mut n_flips := 0
		for k := perm[0]; k != 0; k = perm[0] {
			k2 := (k + 1) >> 1
			for i in 0 .. k2 {
				swap(mut perm, i, k - i)
			}
			n_flips += 1
		}

		if n_flips > n_max_flips {
			n_max_flips = n_flips
		}
		if perm_count % 2 == 0 {
			checksum += n_flips
		} else {
			checksum -= n_flips
		}

		for true {
			if r == n {
				return checksum, n_max_flips
			}

			perm0 := perm1[0]
			for i in 0 .. r {
				swap(mut perm1, i, i + 1)
			}

			perm1[r] = perm0
			cntr := count[r] - 1
			count[r] = cntr
			if cntr > 0 {
				break
			}
			r += 1
		}
		perm_count += 1
	}
	return n, n
}

[direct_array_access; inline]
fn swap(mut array [32]int, i int, j int) {
	tmp := array[i]
	array[i] = array[j]
	array[j] = tmp
}
