import strconv
import os

struct Node {
	left  &Node
	right &Node
}

fn check(node &Node) int {
	mut ret := 1
	unsafe {
		if node.left != 0 {
			ret += check(node.left)
		}
		if node.right != 0 {
			ret += check(node.right)
		}
	}
	return ret
}

fn create(n int) &Node {
	if n == 0 {
		return &Node{
			left: 0
			right: 0
		}
	}
	return &Node{
		left: create(n - 1)
		right: create(n - 1)
	}
}

const min_depth = 4

fn main() {
	mut max_depth := 10
	if os.args.len > 1 {
		max_depth = strconv.atoi(os.args[1]) or { 10 }
	}

	stretch_depth := max_depth + 1
	stretch_tree := create(stretch_depth)
	println('stretch tree of depth $stretch_depth\t check: ${check(stretch_tree)}')

	long_lived_tree := create(max_depth)

	n_results := (max_depth - min_depth) / 2 + 1

	for i in 0 .. n_results {
		depth := i * 2 + min_depth
		n := 1 << (max_depth - depth + min_depth)

		mut check_result := 0
		for _ in 0 .. n {
			node := create(depth)
			check_result += check(node)
		}

		println('$n\t trees of depth $depth\t check: $check_result')
	}

	println('long lived tree of depth $max_depth\t check: ${check(long_lived_tree)}')
}
