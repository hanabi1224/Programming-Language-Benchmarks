module main

import os
import strconv

const line_width = 60

const im = 139968

const ia = 3877

const ic = 29573

const buf_size = 1 << 16

const alu = 'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA'.bytes()

struct AminoAcid {
	c u8
mut:
	p f64
}

// Buffers output, flushing to the OS in 64 KiB blocks.
struct Out {
mut:
	f   os.File = os.stdout()
	buf []u8    = []u8{len: buf_size}
	pos int
}

@[inline]
fn (mut o Out) put(c u8) {
	o.buf[o.pos] = c
	o.pos++
	if o.pos == buf_size {
		o.flush()
	}
}

fn (mut o Out) put_string(s string) {
	for c in s {
		o.put(c)
	}
}

fn (mut o Out) flush() {
	if o.pos > 0 {
		o.f.write(o.buf[..o.pos]) or {}
		o.pos = 0
	}
}

fn main() {
	mut n := 1
	if os.args.len == 2 {
		n = strconv.atoi(os.args[1]) or { 1 }
	}

	mut rand := RandGen{
		seed: 42
	}

	mut iub := [
		AminoAcid{`a`, 0.27},
		AminoAcid{`c`, 0.12},
		AminoAcid{`g`, 0.12},
		AminoAcid{`t`, 0.27},
		AminoAcid{`B`, 0.02},
		AminoAcid{`D`, 0.02},
		AminoAcid{`H`, 0.02},
		AminoAcid{`K`, 0.02},
		AminoAcid{`M`, 0.02},
		AminoAcid{`N`, 0.02},
		AminoAcid{`R`, 0.02},
		AminoAcid{`S`, 0.02},
		AminoAcid{`V`, 0.02},
		AminoAcid{`W`, 0.02},
		AminoAcid{`Y`, 0.02},
	]

	mut homosapiens := [
		AminoAcid{`a`, 0.3029549426680},
		AminoAcid{`c`, 0.1979883004921},
		AminoAcid{`g`, 0.1975473066391},
		AminoAcid{`t`, 0.3015094502008},
	]

	accumulate(mut iub)
	accumulate(mut homosapiens)

	mut o := Out{}
	o.put_string('>ONE Homo sapiens alu\n')
	make_repeat_fasta(mut o, alu, n * 2)
	o.put_string('>TWO IUB ambiguity codes\n')
	make_random_fasta(mut o, mut rand, iub, n * 3)
	o.put_string('>THREE Homo sapiens frequency\n')
	make_random_fasta(mut o, mut rand, homosapiens, n * 5)
	o.flush()
}

fn accumulate(mut table []AminoAcid) {
	for i in 1 .. table.len {
		table[i].p += table[i - 1].p
	}
}

// `src` is repeated once so any 60-byte window can be copied without wrapping.
fn make_repeat_fasta(mut o Out, src []u8, n int) {
	wrapped := []u8{len: src.len + line_width, init: src[index % src.len]}
	mut pos := 0
	mut left := n
	for left > 0 {
		line := if left < line_width { left } else { line_width }
		for i in 0 .. line {
			o.put(wrapped[pos + i])
		}
		o.put(`\n`)
		pos += line
		if pos >= src.len {
			pos -= src.len
		}
		left -= line
	}
}

// The generator has only `im` states and the cumulative probabilities are
// increasing, so resolve every state's pick up front in one monotone sweep.
fn make_random_fasta(mut o Out, mut rand_gen RandGen, table []AminoAcid, n int) {
	mut lut := []u8{len: im}
	mut k := 0
	for s in 0 .. im {
		rand := f64(s) / f64(im)
		for k < table.len && table[k].p <= rand {
			k++
		}
		lut[s] = if k < table.len { table[k].c } else { 0 }
	}

	mut col := 0
	for _ in 0 .. n {
		c := lut[rand_gen.next()]
		if c != 0 {
			o.put(c)
			col++
			if col == line_width {
				o.put(`\n`)
				col = 0
			}
		}
	}
	if col != 0 {
		o.put(`\n`)
	}
}

struct RandGen {
mut:
	seed int
}

@[inline]
fn (mut r RandGen) next() int {
	r.seed = (r.seed * ia + ic) % im
	return r.seed
}
