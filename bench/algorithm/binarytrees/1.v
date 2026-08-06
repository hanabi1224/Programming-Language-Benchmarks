import strconv
import os

struct Node {
	left  &Node = unsafe { nil }
	right &Node = unsafe { nil }
}

fn check(node &Node) int {
	mut ret := 1
	if node.left != nil {
		ret += check(node.left)
	}
	if node.right != nil {
		ret += check(node.right)
	}
	return ret
}

// Nodes are carved out of one block per tree; the block is reset and reused
// between trees of the same depth.
struct Arena {
mut:
	nodes []Node
	pos   int
}

fn new_arena(depth int) Arena {
	return Arena{
		nodes: []Node{len: (1 << (depth + 1)) - 1}
	}
}

@[direct_array_access]
fn (mut a Arena) create(depth int) &Node {
	i := a.pos
	a.pos++
	a.nodes[i] = if depth == 0 {
		Node{}
	} else {
		Node{
			left:  a.create(depth - 1)
			right: a.create(depth - 1)
		}
	}
	return unsafe { &a.nodes[i] }
}

const min_depth = 4

fn main() {
	mut max_depth := 10
	if os.args.len > 1 {
		max_depth = strconv.atoi(os.args[1]) or { 10 }
	}

	stretch_depth := max_depth + 1
	mut stretch_arena := new_arena(stretch_depth)
	stretch_tree := stretch_arena.create(stretch_depth)
	println('stretch tree of depth ${stretch_depth}\t check: ${check(stretch_tree)}')

	mut long_lived_arena := new_arena(max_depth)
	long_lived_tree := long_lived_arena.create(max_depth)

	n_results := (max_depth - min_depth) / 2 + 1

	for i in 0 .. n_results {
		depth := i * 2 + min_depth
		n := 1 << (max_depth - depth + min_depth)

		mut arena := new_arena(depth)
		mut check_result := 0
		for _ in 0 .. n {
			arena.pos = 0
			node := arena.create(depth)
			check_result += check(node)
		}

		println('${n}\t trees of depth ${depth}\t check: ${check_result}')
	}

	println('long lived tree of depth ${max_depth}\t check: ${check(long_lived_tree)}')
}
