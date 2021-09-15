package main

import (
	"fmt"
	"os"
	"strconv"
)

func nsieve(n int) {
	flags := make([]bool, n)
	count := 0
	for i := 2; i < n; i++ {
		if !flags[i] {
			count += 1
			for j := i << 1; j < n; j += i {
				flags[j] = true
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
		nsieve(10000 << (n - i))
	}
}
