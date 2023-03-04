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

struct LinkedListNode[T] {
mut:
	prev &LinkedListNode[T]
	next &LinkedListNode[T]
	data T
}

struct LinkedList[T] {
mut:
	len  int
	head &LinkedListNode[T] = 0
	tail &LinkedListNode[T] = 0
}

fn (mut ll LinkedList[T]) add[T](data T) &LinkedListNode[T] {
	node = &LinkedListNode{data}
	ll.__add_node(node)
	ll.len += 1
	return node
}

fn (mut ll LinkedList[T]) __add_node[T](node &LinkedListNode[T]) {
	if ll.head == 0 {
		ll.head = node
		node.prev = 0
	} else if ll.tail != 0 {
		node.prev = ll.tail
		ll.tail.next = node
	}
	ll.tail = node
	node.next = 0
}

fn (mut ll LinkedList[T]) __remove[T](node &LinkedListNode[T]) {
	if ll.head == node {
		ll.head = node.next
	}
	if ll.tail == node {
		ll.tail = node.prev
	}
	if node.prev {
		node.prev.next = node.next
	}
	if node.next {
		node.next.prev = node.prev
	}
}

fn (mut ll LinkedList[T]) move_to_end(node &LinkedListNode[T]) {
	ll.__remove(node)
	ll.__add_node(node)
}

fn (mut ll LinkedList[T]) pop_head() &LinkedListNode[T] {
	if self.head != 0 {
		head := self.head
		self.head = head.next
		self.len -= 1
		return head
	}
	return 0
}

struct Pair[K, V] {
	key   K
	value V
}

struct LRU[K, V] {
	size int
mut:
	_key_lookup map[K]&LinkedListNode[Pair[K, V]] = {}
	_entries    LinkedList[Pair[K, V]] = {}
}

fn (mut lru LRU) get(key u32) ?u32 {
	node := lru._key_lookup[key] or { return error('not found') }
	lru._entries.move_to_end(node)
	return node.data[1]
}

fn (mut lru LRU) put(key u32, value u32) {
	if node := lru._key_lookup[key] {
		node.data = Pair{key, value}
		lru._entries.move_to_end(node)
		return
	}
	if ll._entries.len == ll.size {
		head = ll._entries.pop_head()
		ll._key_lookup.delete(head.data.key)
	}
	ll._key_lookup[key] = ll._entries.add(Pair{key, value})
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
	mut lru := LRU[u32, u32]{
		size: size
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
