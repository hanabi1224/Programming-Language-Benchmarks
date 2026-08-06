module main

import os
import runtime
import strconv

const max_n = 16

struct Result {
	checksum  int
	max_flips int
}

fn main() {
	mut n := 10
	if os.args.len == 2 {
		n = strconv.atoi(os.args[1]) or { 10 }
	}

	total := factorials(n)[n]

	mut nthreads := runtime.nr_cpus()
	if nthreads < 1 {
		nthreads = 1
	}
	// Chunks must start on an even index: the checksum's sign derives from it.
	nchunks := i64(nthreads) * 4
	mut chunk := (total + nchunks - 1) / nchunks
	chunk += chunk % 2

	mut workers := []thread Result{}
	for lo := i64(0); lo < total; lo += chunk {
		hi := if lo + chunk < total { lo + chunk } else { total }
		workers << spawn fannkuchredux(n, lo, hi)
	}
	results := workers.wait()

	mut checksum := 0
	mut max_flips := 0
	for r in results {
		checksum += r.checksum
		if r.max_flips > max_flips {
			max_flips = r.max_flips
		}
	}

	println('${checksum}\nPfannkuchen(${n}) = ${max_flips}')
}

fn factorials(n int) []i64 {
	mut fact := []i64{len: n + 1}
	fact[0] = 1
	for i in 1 .. n + 1 {
		fact[i] = fact[i - 1] * i
	}
	return fact
}

// Walks permutations [idx_min, idx_max), seeking to the start once and then
// advancing incrementally, and skipping the flip count when p[0] is 0.
fn fannkuchredux(n int, idx_min i64, idx_max i64) Result {
	fact := factorials(n)
	mut p := [max_n]int{}
	mut pp := [max_n]int{}
	mut count := [max_n]int{}

	for i in 0 .. n {
		p[i] = i
	}
	mut seek := idx_min
	for i := n - 1; i > 0; i-- {
		d := int(seek / fact[i])
		count[i] = d
		seek %= fact[i]
		pp = p
		for j in 0 .. i + 1 {
			p[j] = if j + d <= i { pp[j + d] } else { pp[j + d - i - 1] }
		}
	}

	mut max_flips := 1
	mut checksum := 0
	mut sign := true

	for idx := idx_min; true; idx++ {
		first := p[0]
		if first != 0 {
			mut flips := 1
			if p[first] != 0 {
				pp = p
				mut p0 := first
				for {
					flips++
					for i, j := 1, p0 - 1; i < j; i, j = i + 1, j - 1 {
						pp[i], pp[j] = pp[j], pp[i]
					}
					t := pp[p0]
					pp[p0] = p0
					p0 = t
					if pp[p0] == 0 {
						break
					}
				}
			}
			if max_flips < flips {
				max_flips = flips
			}
			if sign {
				checksum += flips
			} else {
				checksum -= flips
			}
		}

		if idx + 1 == idx_max {
			break
		}

		if sign {
			p[0], p[1] = p[1], first
			sign = false
		} else {
			p[1], p[2] = p[2], p[1]
			sign = true
			mut f := first
			for k := 2; true; k++ {
				count[k]++
				if count[k] <= k {
					break
				}
				count[k] = 0
				for j in 0 .. k + 1 {
					p[j] = p[j + 1]
				}
				p[k + 1] = f
				f = p[0]
			}
		}
	}

	return Result{checksum, max_flips}
}
