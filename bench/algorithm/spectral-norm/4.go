/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by K P anonymous
 * corrected by Isaac Gouy
 */

package main

import (
	"flag"
	"fmt"
	"math"
	"runtime"
	"strconv"
)

var n = 0    // var n = flag.Int("n", 2000, "count")
var nCPU = 4 // var nCPU = flag.Int("ncpu", 4, "number of cpus")

type Vec []float64

func (v Vec) Times(ii, n int, u Vec, c chan int) {
	ul := len(u)
	for i := ii; i < n; i++ {
		var vi float64
		for j := 0; j < ul; j++ {
			vi += u[j] / float64(A(i, j))
		}
		v[i] = vi
	}
	c <- 1
}

func (v Vec) TimesTransp(ii, n int, u Vec, c chan int) {
	ul := len(u)
	for i := ii; i < n; i++ {
		var vi float64
		for j := 0; j < ul; j++ {
			vi += u[j] / float64(A(j, i))
		}
		v[i] = vi
	}
	c <- 1
}

func wait(c chan int) {
	for i := 0; i < nCPU; i++ {
		<-c
	}
}

func (v Vec) ATimesTransp(u Vec) {
	x := make(Vec, len(u))
	c := make(chan int, nCPU)
	for i := 0; i < nCPU; i++ {
		go x.Times(i*len(v)/nCPU, (i+1)*len(v)/nCPU, u, c)
	}
	wait(c)
	for i := 0; i < nCPU; i++ {
		go v.TimesTransp(i*len(v)/nCPU, (i+1)*len(v)/nCPU, x, c)
	}
	wait(c)
}

func A(i, j int) int {
	return ((i+j)*(i+j+1)/2 + i + 1)
}

func main() {
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}
	runtime.GOMAXPROCS(nCPU)

	u := make(Vec, n)
	for i := range u {
		u[i] = 1
	}
	v := make(Vec, n)
	for i := 0; i < 10; i++ {
		v.ATimesTransp(u)
		u.ATimesTransp(v)
	}
	var vBv, vv float64
	for i, vi := range v {
		vBv += u[i] * vi
		vv += vi * vi
	}
	fmt.Printf("%0.9f\n", math.Sqrt(vBv/vv))
}
