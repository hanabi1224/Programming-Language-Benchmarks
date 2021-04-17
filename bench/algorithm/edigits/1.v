module main

import os
import strconv
import hanabi1224.biginteger
import math

fn main() {
	mut n := 27
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	k := binary_search(n)
	mut p, q := sum_terms(0, k - 1)
	p += q
	mut a := pow_big(biginteger.ten, u32(n - 1))
	answer := p * a / q
	s := answer.str()
	for i := 0; i < n; i += 10 {
		if i + 10 <= n {
			println('${s[i..i + 10]}\t:${i + 10}')
		} else {
			mut line := s[i..n]
			for _ in 0 .. (10 - n % 10) {
				line = '$line '
			}
			print('$line\t:$n')
		}
	}
}

fn pow_big(a biginteger.BigInteger, b u32) biginteger.BigInteger {
	if b == 0 {
		return biginteger.one
	} else if b == 1 {
		return a
	}

	half := b / 2
	p := pow_big(a, half)
	if half % 2 == 0 {
		return p * p
	} else {
		return p * p * a
	}
}

fn sum_terms(a int, b int) (biginteger.BigInteger, biginteger.BigInteger) {
	if b == a + 1 {
		return biginteger.one, biginteger.from_int(b)
	}
	mid := (a + b) / 2
	p_left, q_left := sum_terms(a, mid)
	p_right, q_right := sum_terms(mid, b)
	return p_left * q_right + p_right, q_left * q_right
}

fn binary_search(n int) int {
	mut a := 0
	mut b := 1
	for !test_k(n, b) {
		a = b
		b *= 2
	}
	for b - a > 1 {
		m := (a + b) / 2
		if test_k(n, m) {
			b = m
		} else {
			a = m
		}
	}
	return b
}

fn test_k(n int, k int) bool {
	if k < 0 {
		return false
	}
	ln_k_factorial := k * (math.log(k) - 1) + 0.5 * math.log(math.tau)
	log_10_k_factorial := ln_k_factorial / math.ln10
	return log_10_k_factorial >= n + 50
}
