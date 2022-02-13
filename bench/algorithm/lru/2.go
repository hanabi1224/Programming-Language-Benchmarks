package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/things-go/container/linkedmap"
)

const (
	A uint32 = 1103515245
	C uint32 = 12345
	M uint32 = 1 << 31
)

func lcg(seed uint32) func() uint32 {
	r := seed
	return func() uint32 {
		r = (A*r + C) % M
		return r
	}
}

type LRU struct {
	Size int
	m    *linkedmap.LinkedMap
}

func newLRU(size int) LRU {
	return LRU{Size: size, m: linkedmap.New()}
}

func (c *LRU) Get(key uint32) (uint32, bool) {
	v := c.m.Get(key, nil)
	if v != nil {
		v := v.(uint32)
		c.m.Remove(key)
		c.m.PushBack(key, v)
		return v, true
	} else {
		return 0, false
	}
}

func (c *LRU) Put(key, value uint32) {
	if c.m.Get(key, nil) != nil {
		c.m.Remove(key)
	} else if c.m.Len() == c.Size {
		c.m.PollFront()
	}
	c.m.PushBack(key, value)
}

func main() {
	size := 100
	if len(os.Args) > 1 {
		if _size, err := strconv.Atoi(os.Args[1]); err == nil {
			size = int(_size)
		}
	}
	n := 10000
	if len(os.Args) > 2 {
		if _n, err := strconv.Atoi(os.Args[2]); err == nil {
			n = int(_n)
		}
	}
	M := uint32(size) * 10
	rng0 := lcg(0)
	rng1 := lcg(1)
	hit := 0
	missed := 0
	lru := newLRU(size)
	for i := 0; i < n; i++ {
		n0 := rng0() % M
		lru.Put(n0, n0)

		n1 := rng1() % M
		if _, ok := lru.Get(n1); ok {
			hit += 1
		} else {
			missed += 1
		}
	}
	fmt.Printf("%d\n%d\n", hit, missed)
}
