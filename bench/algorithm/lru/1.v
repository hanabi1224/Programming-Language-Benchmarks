module main

import os
import strconv

const a = u32(1103515245)
const c = u32(12345)
const m = u32(1) << 31

struct LCG {
mut:
	seed u32
}

fn (mut lcg LCG) next() u32 {
	lcg.seed = (a * lcg.seed + c) % m
	return lcg.seed
}

struct Node {
mut:
	key   u32
	value u32
	prev  int // recency list, towards least recently used
	next  int // recency list, towards most recently used
	hnext int // next entry in this node's hash bucket
}

// Slot 0 is a sentinel the recency list runs circularly through, and it also
// terminates the hash chains, so neither needs a nil check.
struct LRU {
	size  int
	shift u32
mut:
	nodes   []Node
	buckets []int
}

fn new_lru(size int) LRU {
	mut bits := u32(1)
	for (1 << bits) < size * 2 {
		bits++
	}
	mut nodes := []Node{cap: size + 1}
	nodes << Node{}
	return LRU{
		size:    size
		shift:   32 - bits
		nodes:   nodes
		buckets: []int{len: 1 << bits}
	}
}

// Fibonacci hashing: the high bits of the product are the well mixed ones.
@[inline]
fn (lru &LRU) bucket_of(key u32) int {
	return int((key * u32(2654435761)) >> lru.shift)
}

@[direct_array_access]
fn (lru &LRU) find(key u32) int {
	mut i := lru.buckets[lru.bucket_of(key)]
	for i != 0 && lru.nodes[i].key != key {
		i = lru.nodes[i].hnext
	}
	return i
}

@[inline]
fn (mut lru LRU) hash_insert(i int, key u32) {
	b := lru.bucket_of(key)
	lru.nodes[i].hnext = lru.buckets[b]
	lru.buckets[b] = i
}

fn (mut lru LRU) hash_remove(key u32) {
	b := lru.bucket_of(key)
	mut i := lru.buckets[b]
	if lru.nodes[i].key == key {
		lru.buckets[b] = lru.nodes[i].hnext
		return
	}
	for lru.nodes[lru.nodes[i].hnext].key != key {
		i = lru.nodes[i].hnext
	}
	lru.nodes[i].hnext = lru.nodes[lru.nodes[i].hnext].hnext
}

@[direct_array_access; inline]
fn (mut lru LRU) move_to_back(i int) {
	prev := lru.nodes[i].prev
	next := lru.nodes[i].next
	lru.nodes[prev].next = next
	lru.nodes[next].prev = prev
	tail := lru.nodes[0].prev
	lru.nodes[i].prev = tail
	lru.nodes[i].next = 0
	lru.nodes[tail].next = i
	lru.nodes[0].prev = i
}

@[direct_array_access]
fn (mut lru LRU) get(key u32) ?u32 {
	i := lru.find(key)
	if i == 0 {
		return none
	}
	lru.move_to_back(i)
	return lru.nodes[i].value
}

@[direct_array_access]
fn (mut lru LRU) put(key u32, value u32) {
	mut i := lru.find(key)
	if i == 0 {
		if lru.nodes.len > lru.size {
			i = lru.nodes[0].next
			lru.hash_remove(lru.nodes[i].key)
			lru.nodes[i].key = key
		} else {
			i = lru.nodes.len
			// Self-linked, so the unlink inside move_to_back is a no-op.
			lru.nodes << Node{
				key:  key
				prev: i
				next: i
			}
		}
		lru.hash_insert(i, key)
	}
	lru.nodes[i].value = value
	lru.move_to_back(i)
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
	mut lru := new_lru(size)

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
