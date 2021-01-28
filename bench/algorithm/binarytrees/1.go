/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by The Go Authors.
 * based on C program by Kevin Carson
 * flag.Arg hack by Isaac Gouy
 * *reset*
 */

package main

import (
	"flag"
	"fmt"
	"strconv"
)

var n = 0

type Node struct {
	left, right *Node
}

func bottomUpTree(depth int) *Node {
	if depth <= 0 {
		return &Node{}
	}
	return &Node{bottomUpTree(depth - 1), bottomUpTree(depth - 1)}
}

func (n *Node) itemCheck() int {
	if n.left == nil {
		return 1
	}
	return 1 + n.left.itemCheck() + n.right.itemCheck()
}

const minDepth = 4

func main() {
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}

	maxDepth := n
	if minDepth+2 > n {
		maxDepth = minDepth + 2
	}
	stretchDepth := maxDepth + 1

	check := bottomUpTree(stretchDepth).itemCheck()
	fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check)

	longLivedTree := bottomUpTree(maxDepth)

	for depth := minDepth; depth <= maxDepth; depth += 2 {
		iterations := 1 << uint(maxDepth-depth+minDepth)
		check = 0

		for i := 1; i <= iterations; i++ {
			check += bottomUpTree(depth).itemCheck()
		}
		fmt.Printf("%d\t trees of depth %d\t check: %d\n", iterations, depth, check)
	}
	fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, longLivedTree.itemCheck())
}
