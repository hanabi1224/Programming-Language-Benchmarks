// contributed by Branimir Maksimovic

package main

import "core:fmt"
import "core:math"
import "core:mem"
import "core:os"
import "core:strconv"

MIN_DEPTH :: 4
main :: proc() {
	#no_bounds_check n := strconv.parse_int(os.args[1]) or_else 10
	max_depth := math.max(MIN_DEPTH + 2, n)
	{
		stretch_depth := max_depth + 1
		stretch_tree := makeTree(stretch_depth)
		defer {delete_tree(stretch_tree)}
		fmt.printf("stretch tree of depth %d\t check: %d\n", stretch_depth, check(stretch_tree))
	}
	long_lived_tree := makeTree(max_depth)
	defer delete_tree(long_lived_tree)
	depth: int = MIN_DEPTH
	for ; depth <= max_depth; depth += 2 {
		iterations := 1 << u8(max_depth - depth + MIN_DEPTH)
		sum: u64 = 0
		for _ in 0 ..< iterations {
			tree := makeTree(depth)
			defer {delete_tree(tree)}
			sum += u64(check(tree))
		}
		fmt.printf("%d\t trees of depth %d\t check: %d\n", iterations, depth, sum)
	}

	fmt.printf("long lived tree of depth %d\t check: %d\n", max_depth, check(long_lived_tree))
}

Node :: struct {
	left, right: ^Node,
}

makeTree :: proc(depth: int) -> ^Node {
	node := new(Node)
	if node == nil {
		fmt.println("alloc error")
		return node
	}
	if depth > 0 {
		node.left = makeTree(depth - 1)
		node.right = makeTree(depth - 1)
	}
	return node
}
delete_tree :: proc(t: ^Node) {
	if t.left != nil {delete_tree(t.left)}
	if t.right != nil {delete_tree(t.right)}
	free(t)
}
check :: proc(tree: ^Node) -> int {
	sum: int = 1
	if tree == nil {return 0}
	sum += check(tree.left)
	sum += check(tree.right)
	return sum
}
