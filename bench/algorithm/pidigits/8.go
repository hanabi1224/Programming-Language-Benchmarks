/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * based on pidigits.c (by Paolo Bonzini & Sean Bartlett,
 *                      modified by Michael Mellor)
 *
 * contributed by The Go Authors.
 * line printer hack by Sean Lake
 * modified by Bert Gijsbers
 * make pidigits.go #2 use "math/big"
 */

package main

import (
	"bufio"
	"flag"
	"fmt"
	"math/big"
	"os"
	"runtime"
	"strconv"
)

var n = 0
var silent = false

var (
	tmp1  = big.NewInt(0)
	tmp2  = big.NewInt(0)
	y2    = big.NewInt(1)
	bigk  = big.NewInt(0)
	accum = big.NewInt(0)
	denom = big.NewInt(1)
	numer = big.NewInt(1)
	ten   = big.NewInt(10)
	three = big.NewInt(3)
	four  = big.NewInt(4)
)

func next_term(k int64) int64 {
	for {
		k++
		y2.SetInt64(k*2 + 1)
		bigk.SetInt64(k)

		tmp1.Lsh(numer, 1)
		accum.Add(accum, tmp1)
		accum.Mul(accum, y2)
		denom.Mul(denom, y2)
		numer.Mul(numer, bigk)

		if accum.Cmp(numer) > 0 {
			return k
		}
	}
}

func extract_digit(nth *big.Int) int64 {
	tmp1.Mul(nth, numer)
	tmp2.Add(tmp1, accum)
	tmp1.Div(tmp2, denom)
	return tmp1.Int64()
}

func next_digit(k int64) (int64, int64) {
	for {
		k = next_term(k)
		d3 := extract_digit(three)
		d4 := extract_digit(four)
		if d3 == d4 {
			return d3, k
		}
	}
}

func eliminate_digit(d int64) {
	tmp1.SetInt64(d)
	accum.Sub(accum, tmp1.Mul(denom, tmp1))
	accum.Mul(accum, ten)
	numer.Mul(numer, ten)
}

func init() {
	runtime.GOMAXPROCS(1)
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}
}

func main() {
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	line := make([]byte, 0, 10)
	var d, k int64
	for i := 1; i <= n; i++ {
		d, k = next_digit(k)
		line = append(line, byte(d)+'0')
		if len(line) == 10 {
			if silent != true {
				fmt.Fprintf(w, "%s\t:%d\n", string(line), i)
			}
			line = line[:0]
		}
		eliminate_digit(d)
	}
	if len(line) > 0 && silent != true {
		fmt.Fprintf(w, "%-10s\t:%d\n", string(line), n)
	}
}
