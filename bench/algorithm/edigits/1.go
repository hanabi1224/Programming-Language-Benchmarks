package main

import (
	"bufio"
	"fmt"
	"math"
	"math/big"
	"os"
	"strconv"
)

var (
	one = big.NewInt(1)
)

func main() {
	n := int64(27)
	if len(os.Args) > 1 {
		if _n, err := strconv.Atoi(os.Args[1]); err == nil {
			n = int64(_n)
		}
	}
	k := binary_search(n)
	p, q := sum_terms(0, k-1)
	p.Add(p, q)
	a := big.NewInt(10)
	a.Exp(a, big.NewInt(n-1), nil)
	answer := p
	answer.Mul(answer, a)
	answer.Div(answer, q)
	s := answer.String()
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for i := int64(0); i < n; i += 10 {
		if i+10 <= n {
			fmt.Fprintf(out, "%s\t:%d\n", s[i:i+10], i+10)
		} else {
			fmt.Fprintf(out, "%-10s\t:%d\n", s[i:n], n)
		}
	}
}

func sum_terms(a int64, b int64) (*big.Int, *big.Int) {
	if b == a+1 {
		return one, big.NewInt(b)
	}
	mid := (a + b) / 2
	p_left, q_left := sum_terms(a, mid)
	p_right, q_right := sum_terms(mid, b)
	l := big.NewInt(0)
	r := big.NewInt(0)
	l.Mul(p_left, q_right)
	l.Add(l, p_right)
	r.Mul(q_left, q_right)
	return l, r
}

func binary_search(n int64) int64 {
	a := int64(0)
	b := int64(1)
	for !test_k(n, b) {
		a = b
		b *= 2
	}
	for b-a > 1 {
		m := (a + b) / 2
		if test_k(n, m) {
			b = m
		} else {
			a = m
		}
	}
	return b
}

func test_k(n int64, k int64) bool {
	if k < 0 {
		return false
	}
	ln_k_factorial := float64(k)*(math.Log(float64(k))-1) + 0.5*math.Log(math.Pi*2)
	log_10_k_factorial := ln_k_factorial / math.Ln10
	return log_10_k_factorial >= float64(n+50)
}
