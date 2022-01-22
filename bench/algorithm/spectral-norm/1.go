package main

import (
	"flag"
	"fmt"
	"math"
	"strconv"
)

var n = 0

type Vec []float64

func evala(i int, j int) int {
	return (i+j)*(i+j+1)/2 + i + 1
}

func times(v, u Vec) {
	for i := 0; i < len(v); i++ {
		a := 0.0
		for j := 0; j < len(u); j++ {
			a += u[j] / float64(evala(i, j))
		}
		v[i] = a
	}
}

func times_trans(v, u Vec) {
	for i := 0; i < len(v); i++ {
		a := 0.0
		for j := 0; j < len(u); j++ {
			a += u[j] / float64(evala(j, i))
		}
		v[i] = a
	}
}

func a_times_transp(v, u Vec) {
	x := make(Vec, len(u))
	times(x, u)
	times_trans(v, x)
}

func main() {
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}

	u := make(Vec, n)
	v := make(Vec, n)
	for i := range u {
		u[i] = 1
		v[i] = 1
	}
	for i := 0; i < 10; i++ {
		a_times_transp(v, u)
		a_times_transp(u, v)
	}
	var vBv, vv float64
	for i, vi := range v {
		vBv += u[i] * vi
		vv += vi * vi
	}
	fmt.Printf("%0.9f\n", math.Sqrt(vBv/vv))
}
