package main

import (
    "fmt"
    "os"
    "strconv"
    "sync"
)

type Node struct {
    left  *Node
    right *Node
}

func bottomUpTree(depth int) *Node {
    if depth <= 0 {
        return &Node{}
    }
    n := &Node{}
    n.left = bottomUpTree(depth - 1)
    n.right = n.left 
    return n
}

func (n *Node) itemCheck() int {
    if n.left == nil {
        return 1
    }
    return 1 + n.left.itemCheck()*2
}

const minDepth = 4

func main() {
    var maxDepth int
    if len(os.Args) > 1 {
        var err error
        if maxDepth, err = strconv.Atoi(os.Args[1]); err != nil {
            maxDepth = 5 
        }
    } else {
        maxDepth = 5
    }

    if minDepth+2 > maxDepth {
        maxDepth = minDepth + 2
    }
    stretchDepth := maxDepth + 1

    check := bottomUpTree(stretchDepth).itemCheck()
    fmt.Printf("stretch tree of depth %d\t check: %d\n", stretchDepth, check)

    longLivedTree := bottomUpTree(maxDepth)

    var wg sync.WaitGroup
    for depth := minDepth; depth <= maxDepth; depth += 2 {
        depth := depth // captura a variável para o escopo do loop
        wg.Add(1)
        go func() {
            defer wg.Done()
            iterations := 1 << uint(maxDepth-depth+minDepth)
            check := 0

            for i := 0; i < iterations; i++ {
                check += bottomUpTree(depth).itemCheck()
            }
            fmt.Printf("%d\t trees of depth %d\t check: %d\n", iterations, depth, check)
        }()
    }
    wg.Wait()
    fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, longLivedTree.itemCheck())
}
