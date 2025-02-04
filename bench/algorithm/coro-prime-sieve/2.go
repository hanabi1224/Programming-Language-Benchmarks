package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

//        get r3kt.
//    - yung innanet

func SieveOfEratosthenes(out io.Writer) {
	sieve := make([]bool, 20000)
	p := 2

	for {
		if !sieve[p] {
			_, _ = fmt.Fprintln(out, p)
			for multiple := p * p; multiple <= len(sieve)-1; multiple += p {
				sieve[multiple] = true
			}
		}
		p++
		if p > len(sieve)-1 {
			return
		}
	}
}

func main() {
	n := 1000
	if len(os.Args) > 1 {
		if _n, err := strconv.Atoi(os.Args[1]); err == nil {
			n = _n
		}
	}
	pipeIn, pipeOut := io.Pipe()

	out := bufio.NewWriter(os.Stdout)
	in := bufio.NewWriter(pipeOut)

	readerUntil := bufio.NewReader(pipeIn)
	go SieveOfEratosthenes(in)

	rd := func() []byte {
		b, _ := readerUntil.ReadSlice('\n')
		return b
	}

	i := 0
	for {
		if i >= n {
			break
		}
		_, _ = out.Write(rd())
		i++
	}
	_ = out.Flush()
}
