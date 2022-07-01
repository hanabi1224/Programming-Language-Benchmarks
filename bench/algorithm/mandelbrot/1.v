module main

import os
import strconv
import crypto.md5

[direct_array_access]
fn main() {
	mut n := 1
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}
	n = (n + 7) / 8 * 8
	chunk_size := n / 8
	inv := 2.0 / f64(n)
	mut xloc := [][8]f64{len: chunk_size, cap: chunk_size}
	for i in 0 .. n {
		xloc[i / 8][i % 8] = f64(i) * inv - 1.5
	}
	println('P4\n$n $n')

	mut rows := []byte{len: n * chunk_size, cap: n * chunk_size, init: 0}
	for chunk_id := 0; chunk_id < n; chunk_id++ {
		ci := f64(chunk_id) * inv - 1.0
		for i := 0; i < chunk_size; i++ {
			r := mbrot8(xloc[i], ci)
			if r > 0 {
				rows[chunk_id * chunk_size + i] = r
			}
		}
	}
	println(md5.sum(rows).hex())
}

[direct_array_access]
fn mbrot8(cr [8]f64, civ f64) byte {
	ci := [8]f64{init: civ}
	mut zr := [8]f64{init: 0.0}
	mut zi := [8]f64{init: 0.0}
	mut tr := [8]f64{init: 0.0}
	mut ti := [8]f64{init: 0.0}
	mut absz := [8]f64{init: 0.0}
	mut tmp := [8]f64{}
	for _ in 0 .. 10 {
		for _ in 0 .. 5 {
			// zi = add(mul(add(zr, zr), zi), ci)			
			add(zr, zr, mut tmp)
			mul(tmp, zi, mut tmp)
			add(tmp, ci, mut zi)

			// zr = add(minus(tr, ti), cr)
			minus(tr, ti, mut tmp)
			add(tmp, cr, mut zr)

			mul(zr, zr, mut tr)
			mul(zi, zi, mut ti)
		}
		add(tr, ti, mut absz)
		mut terminate := true
		for i in 0 .. 8 {
			if absz[i] <= 4.0 {
				terminate = false
				break
			}
		}
		if terminate {
			return 0
		}
	}
	mut accu := u8(0)
	for i in 0 .. 8 {
		accu |= u8(if absz[i] <= 4.0 { 0x80 >> i } else { 0 })
	}
	return accu
}

[direct_array_access; inline]
fn add(a [8]f64, b [8]f64, mut r [8]f64) {
	for i in 0 .. 8 {
		r[i] = a[i] + b[i]
	}
}

[direct_array_access; inline]
fn minus(a [8]f64, b [8]f64, mut r [8]f64) {
	for i in 0 .. 8 {
		r[i] = a[i] - b[i]
	}
}

[direct_array_access; inline]
fn mul(a [8]f64, b [8]f64, mut r [8]f64) {
	for i in 0 .. 8 {
		r[i] = a[i] * b[i]
	}
}
