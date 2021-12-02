// A concurrent prime sieve from go officical example

package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// Send the sequence 2, 3, 4, ... to channel 'ch'.
func Generate(ch chan<- int) {
	for i := 2; ; i++ {
		ch <- i // Send 'i' to channel 'ch'.
	}
}

// Copy the values from channel 'in' to channel 'out',
// removing those divisible by 'prime'.
func Filter(in <-chan int, out chan<- int, prime int) {
	for {
		i := <-in // Receive value from 'in'.
		if i%prime != 0 {
			out <- i // Send 'i' to 'out'.
		}
	}
}

// The prime sieve: Daisy-chain Filter processes.
func main() {
	n := 1000
	if len(os.Args) > 1 {
		if _n, err := strconv.Atoi(os.Args[1]); err == nil {
			n = _n
		}
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	ch := make(chan int, 1) // Create a new channel.
	go Generate(ch)         // Launch Generate goroutine.
	for i := 0; i < n; i++ {
		prime := <-ch
		fmt.Fprintln(out, prime)
		if i >= n-1 {
			break
		}
		ch1 := make(chan int, 1)
		go Filter(ch, ch1, prime)
		ch = ch1
	}
}
