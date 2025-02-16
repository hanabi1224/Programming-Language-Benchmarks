package main

import "base:runtime"
import "core:container/lru"
import "core:fmt"
import "core:strconv"

A: uint : 1103515245
C: uint : 12345
M: uint : 1 << 31
LCG :: struct {
	seed: uint,
}
next_lcg :: proc(lcg: ^LCG) -> uint {
	seed := (A * lcg.seed + C) % M
	lcg.seed = seed
	return seed
}

main :: proc() {
	args := runtime.args__
	#no_bounds_check size := strconv.parse_uint(auto_cast args[1]) or_else 100
	#no_bounds_check n := strconv.parse_int(auto_cast args[2]) or_else 100
	mod := size * 10
	rng0 := &LCG{0}
	rng1 := &LCG{1}
	cache := new(lru.Cache(uint, uint))
	defer free(cache)
	lru.init(cache, auto_cast size)
	hit := 0
	missed := 0
	for i in 0 ..< n {
		n0 := next_lcg(rng0) % mod
		lru.set(cache, n0, n0)
		n1 := next_lcg(rng1) % mod
		if _, ok := lru.get(cache, n1); ok {
			hit += 1
		} else {
			missed += 1
		}
	}
	fmt.println(hit)
	fmt.println(missed)
}
