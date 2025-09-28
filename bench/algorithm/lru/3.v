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


struct Pair[K,V] {
mut:
	k K
	v V
}

struct ListNode[K,V] {
mut:
	data Pair[K,V]
	next &ListNode[K,V] = unsafe{nil}
	prev &ListNode[K,V] = unsafe{nil}
}

struct LinkedList[K,V] {
mut:
	head &ListNode[K,V] = unsafe { nil }
	tail &ListNode[K,V] = unsafe { nil }
	len  int
}

struct LRU[K,V] {
	size int
mut:
	keys    map[K] &ListNode[K,V]
	entries LinkedList[K,V]
}


fn (mut list LinkedList[K,V]) add(data Pair[K,V]) &ListNode[K,V] {
	mut node := &ListNode[K,V]{ data: data }
	list.add_node(mut *node)
	list.len += 1
	return node
}


fn (mut list LinkedList[K,V]) add_node(mut node ListNode[K,V]) {
	mut tail := list.tail
	if list.head == unsafe { nil } { list.head = node }
	else if tail != unsafe { nil } {
		node.prev = tail
		tail.next = node
	}
	list.tail = node
	node.next = unsafe{ nil }
}


fn (mut list LinkedList[K,V]) remove(mut node ListNode[K,V]) {
	if list.head == node { list.head = node.next }
	if list.tail == node { list.tail = node.prev }
	if node.prev != unsafe { nil } { node.prev.next = node.next }
	if node.next != unsafe { nil } { node.next.prev = node.prev }
}


fn (mut list LinkedList[K,V]) move_to_end(mut node ListNode[K,V]) {
	list.remove(mut *node)
	list.add_node(mut *node)
}


fn (mut lru LRU[K, V]) get(key K) &V {
	mut node := lru.keys[key] or { return unsafe{ nil } }
	lru.entries.move_to_end(mut node)
	return &node.data.v
}


fn (mut lru LRU[K, V]) put(key K, value V) {
	mut node := lru.keys[key] or {
		if lru.entries.len == lru.size {
			mut head := lru.entries.head
			lru.keys.delete(head.data.k)
			head.data.k = key; head.data.v = value
			lru.keys[key] = head
			lru.entries.move_to_end(mut head)
			return
		}
		lru.keys[key] = lru.entries.add(Pair[K, V] { k: key, v: value })
		return
	}
	node.data.v = value
	lru.entries.move_to_end(mut node)
}


fn main() {
	mut size := 100
	if os.args.len > 1 { size = strconv.atoi(os.args[1]) or { size } }
	
	mut n := 100
	if os.args.len > 2 { n = strconv.atoi(os.args[2]) or { n } }
	mod := u32(size) * 10

	mut rng0 := LCG{ seed: 0 }
	mut rng1 := LCG{ seed: 1 }
	mut lru := LRU[u32, u32] { size: size, keys : {}, entries: LinkedList[u32, u32]{} }

	mut hit := 0
	mut missed := 0

	for _ in 0 .. n {
		n0 := rng0.next() % mod
		lru.put(n0, n0)
		n1 := rng1.next() % mod
		if lru.get(n1) != unsafe{ nil } { hit += 1 }
		else { missed += 1 }
	}
	println('${hit}\n${missed}')
}
