package main

import (
	"fmt"
	"os"
	"strconv"
)

type BaseNode struct {
	hash *int64
}

func (n *BaseNode) HasHash() bool {
	return n.hash != nil
}

func (n *BaseNode) GetHash() int64 {
	if n.hash == nil {
		return -1
	}
	return *n.hash
}

type INode interface {
	CalHash()
	HasHash() bool
	GetHash() int64
	Check() bool
}

type Leaf struct {
	BaseNode
	value int64
}

func (l *Leaf) CalHash() {
	if l.hash == nil {
		l.hash = &l.value
	}
}

func (l *Leaf) Check() bool {
	return l.HasHash()
}

type Node struct {
	BaseNode
	left  INode
	right INode
}

func (n *Node) CalHash() {
	if n.hash == nil {
		n.left.CalHash()
		n.right.CalHash()
		merged := n.left.GetHash() + n.right.GetHash()
		n.hash = &merged
	}
}

func (n *Node) Check() bool {
	if n.HasHash() {
		return n.left.Check() && n.right.Check()
	} else {
		return false
	}
}

func make(depth int) INode {
	if depth <= 0 {
		return &Leaf{value: 1}
	}
	d := depth - 1
	return &Node{left: make(d), right: make(d)}
}

const minDepth = 4

func main() {
	maxDepth := 5
	if len(os.Args) > 1 {
		if _n, err := strconv.Atoi(os.Args[1]); err == nil {
			maxDepth = _n
		}
	}

	if minDepth+2 > maxDepth {
		maxDepth = minDepth + 2
	}
	stretchDepth := maxDepth + 1

	stretchTree := make(stretchDepth)
	stretchTree.CalHash()
	fmt.Printf("stretch tree of depth %d\t root hash: %d check: %t\n", stretchDepth, stretchTree.GetHash(), stretchTree.Check())

	longLivedTree := make(maxDepth)

	for depth := minDepth; depth <= maxDepth; depth += 2 {
		iterations := 1 << uint(maxDepth-depth+minDepth)
		var sum int64 = 0
		for i := 1; i <= iterations; i++ {
			tree := make(depth)
			tree.CalHash()
			sum += tree.GetHash()
		}
		fmt.Printf("%d\t trees of depth %d\t root hash sum: %d\n", iterations, depth, sum)
	}
	longLivedTree.CalHash()
	fmt.Printf("long lived tree of depth %d\t root hash: %d check: %t\n", maxDepth, longLivedTree.GetHash(), longLivedTree.Check())
}
