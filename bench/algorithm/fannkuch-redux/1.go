/*
 * The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Oleg Mazurov, June 2010
 * flag.Arg hack by Isaac Gouy
 *
 */

package main

import (
	"flag"
	"fmt"
	"runtime"
	"strconv"
)

type Result struct {
	maxFlips int
	checkSum int
}

var (
	NCHUNKS = 720
	CHUNKSZ = 0
	NTASKS  = 0
)
var n = 12
var Fact []int

func fannkuch(idxMin int, ch chan Result) {

	idxMax := idxMin + CHUNKSZ
	if idxMax < Fact[n] {
		go fannkuch(idxMax, ch)
	} else {
		idxMax = Fact[n]
	}

	p := make([]int, n)
	pp := make([]int, n)
	count := make([]int, n)

	// first permutation
	for i := 0; i < n; i++ {
		p[i] = i
	}
	for i, idx := n-1, idxMin; i > 0; i-- {
		d := idx / Fact[i]
		count[i] = d
		idx = idx % Fact[i]

		copy(pp, p)
		for j := 0; j <= i; j++ {
			if j+d <= i {
				p[j] = pp[j+d]
			} else {
				p[j] = pp[j+d-i-1]
			}
		}
	}

	maxFlips := 1
	checkSum := 0

	for idx, sign := idxMin, true; ; sign = !sign {

		// count flips
		first := p[0]
		if first != 0 {
			flips := 1
			if p[first] != 0 {
				copy(pp, p)
				p0 := first
				for {
					flips++
					for i, j := 1, p0-1; i < j; i, j = i+1, j-1 {
						pp[i], pp[j] = pp[j], pp[i]
					}
					t := pp[p0]
					pp[p0] = p0
					p0 = t
					if pp[p0] == 0 {
						break
					}
				}
			}
			if maxFlips < flips {
				maxFlips = flips
			}
			if sign {
				checkSum += flips
			} else {
				checkSum -= flips
			}
		}

		if idx++; idx == idxMax {
			break
		}

		// next permutation
		if sign {
			p[0], p[1] = p[1], first
		} else {
			p[1], p[2] = p[2], p[1]
			for k := 2; ; k++ {
				if count[k]++; count[k] <= k {
					break
				}
				count[k] = 0
				for j := 0; j <= k; j++ {
					p[j] = p[j+1]
				}
				p[k+1] = first
				first = p[0]
			}
		}
	}

	ch <- Result{maxFlips, checkSum}
}

func printResult(n int, res int, chk int) {
	fmt.Printf("%d\nPfannkuchen(%d) = %d\n", chk, n, res)
}

func main() {
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}
	runtime.GOMAXPROCS(4)

	Fact = make([]int, n+1)
	Fact[0] = 1
	for i := 1; i < len(Fact); i++ {
		Fact[i] = Fact[i-1] * i
	}

	CHUNKSZ = (Fact[n] + NCHUNKS - 1) / NCHUNKS
	CHUNKSZ += CHUNKSZ % 2
	NTASKS = (Fact[n] + CHUNKSZ - 1) / CHUNKSZ

	ch := make(chan Result, NTASKS)

	go fannkuch(0, ch)

	res := 0
	chk := 0
	for i := 0; i < NTASKS; i++ {
		r := <-ch
		if res < r.maxFlips {
			res = r.maxFlips
		}
		chk += r.checkSum
	}

	printResult(n, res, chk)
}
