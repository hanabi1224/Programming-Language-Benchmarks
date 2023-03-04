module main

import os
import strconv
import math.big
import math

const (
	one = big.integer_from_int(1)
	ten = big.integer_from_int(10)
)

fn main() {
	mut n := 27
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	k := binary_search(n)
	mut p, q := sum_terms(0, k - 1)
	p += q
	mut a := ten.pow(u32(n - 1))
	answer := p * a / q
	s := answer.str()
	for i := 0; i < n; i += 10 {
		if i + 10 <= n {
			println('${s[i..i + 10]}\t:${i + 10}')
		} else {
			mut line := s[i..n]
			for _ in 0 .. (10 - n % 10) {
				line = '${line} '
			}
			print('${line}\t:${n}')
		}
	}
}

fn sum_terms(a int, b int) (big.Integer, big.Integer) {
	if b == a + 1 {
		return one, big.integer_from_int(b)
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
