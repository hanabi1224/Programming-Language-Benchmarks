module main

import os
import strconv

const (
	a = u32(1103515245)
	c = u32(12345)
	m = u32(1) << 31
)

struct LCG {
mut:
	seed u32
}

fn (mut lcg LCG) next() u32 {
	lcg.seed = (a * lcg.seed + c) % m
	return lcg.seed
}

struct LRU {
	size int
mut:
	m map[u32]u32
}

fn (mut lru LRU) get(key u32) ?u32 {
	v := lru.m[key] or { return error('not found') }
	lru.m.delete(key)
	lru.m[key] = v
	return v
}

fn (mut lru LRU) put(key u32, value u32) {
	if key in lru.m {
		lru.m.delete(key)
	} else if lru.m.len == lru.size {
		for k, _ in lru.m {
			lru.m.delete(k)
			break
		}
	}
	lru.m[key] = value
}

fn main() {
	mut size := 100
	if os.args.len > 1 {
		size = strconv.atoi(os.args[1]) or { size }
	}
	mut n := 100
	if os.args.len > 2 {
		n = strconv.atoi(os.args[2]) or { n }
	}
	mod := u32(size) * 10

	mut rng0 := LCG{
		seed: 0
	}
	mut rng1 := LCG{
		seed: 1
	}
	mut lru := LRU{
		size: size
		m: {}
	}

	mut hit := 0
	mut missed := 0

	for _ in 0 .. n {
		n0 := rng0.next() % mod
		lru.put(n0, n0)
		n1 := rng1.next() % mod
		if _ := lru.get(n1) {
			hit += 1
		} else {
			missed += 1
		}
	}

	println('${hit}\n${missed}')
}
