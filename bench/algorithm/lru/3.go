package main

import (
	"container/list"
	"fmt"
	"os"
	"strconv"
)

const (
	A uint32 = 1103515245
	C uint32 = 12345
	M uint32 = 1 << 31
)

type Pair = struct {
	Key, Value uint32
}

func lcg(seed uint32) func() uint32 {
	r := seed
	return func() uint32 {
		r = (A*r + C) % M
		return r
	}
}

type LRU struct {
	Size int

	keys    map[uint32]*list.Element
	entries *list.List
}

func newLRU(size int) LRU {
	return LRU{Size: size, keys: make(map[uint32]*list.Element, size), entries: list.New()}
}

func (c *LRU) Get(key uint32) (uint32, bool) {
	ele, ok := c.keys[key]
	if ok {
		c.entries.MoveToBack(ele)
		return ele.Value.(Pair).Value, true
	} else {
		return 0, false
	}
}

func (c *LRU) Put(key, value uint32) {
	pair := Pair{key, value}
	ele, ok := c.keys[key]
	if ok {
		ele.Value = pair
		c.entries.MoveToBack(ele)
		return
	} else if c.entries.Len() == c.Size {
		ele := c.entries.Front()
		delete(c.keys, ele.Value.(Pair).Key)
		c.entries.Remove(ele)
	}
	c.keys[key] = c.entries.PushBack(pair)
}

func main() {
	n := 10000
	if len(os.Args) > 1 {
		if _n, err := strconv.Atoi(os.Args[1]); err == nil {
			n = int(_n)
		}
	}
	rng0 := lcg(0)
	rng1 := lcg(1)
	hit := 0
	missed := 0
	lru := newLRU(10)
	for i := 0; i < n; i++ {
		n0 := rng0() % 100
		lru.Put(n0, n0)

		n1 := rng1() % 100
		if _, ok := lru.Get(n1); ok {
			hit += 1
		} else {
			missed += 1
		}
	}
	fmt.Printf("%d\n%d\n", hit, missed)
}
