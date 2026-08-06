module main

import os
import strconv
import math.big
import strings
import math

const one = big.integer_from_int(1)
const ten = big.integer_from_int(10)

// Digits per leaf of `write_dec`; 10^leaf_digits - 1 still fits in an u32.
const leaf_digits = 9

fn main() {
	mut n := 27
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	k := binary_search(n)
	mut p, q := sum_terms(0, k - 1)
	p += q
	a := ten.pow(u32(n - 1))
	answer := p * a / q
	s := decimal_str(answer, n)
	mut sb := strings.new_builder(n / 10 * 24)
	for i := 0; i < n; i += 10 {
		if i + 10 <= n {
			sb.writeln('${s[i..i + 10]}\t:${i + 10}')
		} else {
			mut line := s[i..n]
			for _ in 0 .. (10 - n % 10) {
				line = '${line} '
			}
			sb.write_string('${line}\t:${n}')
		}
	}
	print(sb.str())
}

// Renders `x`, which is known to have exactly `n` decimal digits, by
// recursively halving it against precomputed powers of ten.
fn decimal_str(x big.Integer, n int) string {
	mut pows := [ten.pow(leaf_digits)]
	mut width := leaf_digits
	for width < n {
		pows << pows.last() * pows.last()
		width *= 2
	}
	mut buf := []u8{len: width}
	write_dec(x, pows.len - 1, pows, mut buf)
	return buf[width - n..].bytestr()
}

fn write_dec(x big.Integer, level int, pows []big.Integer, mut buf []u8) {
	if x.signum == 0 {
		for i in 0 .. buf.len {
			buf[i] = `0`
		}
		return
	}
	if level == 0 {
		mut v := u32(x.int())
		for i := leaf_digits - 1; i >= 0; i-- {
			buf[i] = u8(`0` + v % 10)
			v /= 10
		}
		return
	}
	hi, lo := x.div_mod(pows[level - 1])
	half := leaf_digits << (level - 1)
	write_dec(hi, level - 1, pows, mut buf[..half])
	write_dec(lo, level - 1, pows, mut buf[half..])
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
