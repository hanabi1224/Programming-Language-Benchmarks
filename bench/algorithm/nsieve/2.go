package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/bits-and-blooms/bitset"
)

func nsieve(n uint) {
	var flags bitset.BitSet
	count := 0
	for i := uint(2); i < n; i++ {
		if !flags.Test(i) {
			count += 1
			for j := i << 1; j < n; j += i {
				flags.Set(j)
			}
		}
	}
	fmt.Printf("Primes up to %8d %8d\n", n, count)
}

func main() {
	n := 4
	if len(os.Args) > 1 {
		n, _ = strconv.Atoi(os.Args[1])
	}
	for i := 0; i < 3; i++ {
		nsieve(uint(10000 << (n - i)))
	}
}
