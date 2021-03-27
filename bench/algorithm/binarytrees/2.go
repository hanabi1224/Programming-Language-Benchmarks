package main

import (
	"flag"
	"fmt"
	"sort"
	"strconv"
	"sync"
)

type Node struct {
	left  *Node
	right *Node
}

func createTree(depth int) *Node {
	if depth > 1 {
		return &Node{createTree(depth - 1), createTree(depth - 1)}
	}
	return &Node{&Node{}, &Node{}}
}

func (node *Node) checkTree() int {
	sum := 1
	if node.left != nil {
		sum += node.left.checkTree()
	}
	if node.right != nil {
		sum += node.right.checkTree()
	}
	return sum
}

func main() {
	n := 10
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}
	run(n)
}

func run(maxDepth int) {
	const minDepth = 4
	var longLivedTree *Node
	var group sync.WaitGroup
	var messages sync.Map

	if minDepth+2 > maxDepth {
		maxDepth = minDepth + 2
	}

	group.Add(1)
	go func() {
		messages.Store(-1, fmt.Sprintf("stretch tree of depth %d\t check: %d",
			maxDepth+1, createTree(maxDepth+1).checkTree()))
		longLivedTree = createTree(maxDepth)
		group.Done()
	}()

	for halfDepth := minDepth / 2; halfDepth < maxDepth/2+1; halfDepth++ {
		iters := 1 << (maxDepth - (halfDepth * 2) + minDepth)
		group.Add(1)
		go func(depth, iters, chk int) {
			for i := 0; i < iters; i++ {
				chk += createTree(depth).checkTree()
			}
			messages.Store(depth, fmt.Sprintf("%d\t trees of depth %d\t check: %d",
				iters, depth, chk))
			group.Done()
		}(halfDepth*2, iters, 0)
	}

	group.Wait()

	var idxs []int
	messages.Range(func(key, val interface{}) bool {
		idxs = append(idxs, key.(int))
		return true
	})
	sort.Ints(idxs)
	for _, idx := range idxs {
		msg, _ := messages.Load(idx)
		fmt.Println(msg)
	}

	fmt.Printf("long lived tree of depth %d\t check: %d\n",
		maxDepth, longLivedTree.checkTree())
}
