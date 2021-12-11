/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * regex-dna program contributed by The Go Authors.
 * modified by Tylor Arndt.
 * modified by Chandra Sekar S to use optimized PCRE binding.
 * modified by Matt Dellandrea.
 * modified by Pavel Griaznov to use PCRE JIT compilation.
 */

package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"runtime"

	"github.com/GRbit/go-pcre"
)

type substitution struct {
	pattern     string
	replacement string
}

func countMatches(pat string, b []byte) int {
	m := pcre.MustCompileJIT(pat, 0, pcre.STUDY_JIT_COMPILE).Matcher(b, 0)
	n := 0

	for f := m.Matches; f; f = m.Match(b, 0) {
		n++

		b = b[m.Index()[1]:]
	}

	return n
}

func main() {
	fileName := "25000_in"
	if len(os.Args) > 1 {
		fileName = os.Args[1]
	}

	var variants = []string{
		"agggtaaa|tttaccct",
		"[cgt]gggtaaa|tttaccc[acg]",
		"a[act]ggtaaa|tttacc[agt]t",
		"ag[act]gtaaa|tttac[agt]ct",
		"agg[act]taaa|ttta[agt]cct",
		"aggg[acg]aaa|ttt[cgt]ccct",
		"agggt[cgt]aa|tt[acg]accct",
		"agggta[cgt]a|t[acg]taccct",
		"agggtaa[cgt]|[acg]ttaccct",
	}

	var substs = []substitution{
		{"tHa[Nt]", "<4>"},
		{"aND|caN|Ha[DS]|WaS", "<3>"},
		{"a[NSt]|BY", "<2>"},
		{"<[^>]*>", "|"},
		{"\\|[^|][^|]*\\|", "-"},
	}

	runtime.GOMAXPROCS(runtime.NumCPU())

	if f, err := os.Open(fileName); err == nil {
		defer f.Close()
		b, err := ioutil.ReadAll(f)
		if err != nil {
			fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
			os.Exit(2)
		}

		ilen := len(b)

		// Delete the comment lines and newlines
		b = pcre.
			MustCompileJIT("(>[^\n]*)?\n", 0, pcre.STUDY_JIT_COMPILE).
			ReplaceAll(b, []byte{}, 0)
		clen := len(b)

		mresults := make([]chan int, len(variants))
		for i := 0; i < len(variants); i++ {
			mresults[i] = make(chan int)

			go func(ch chan int, s string) {
				ch <- countMatches(s, b)
			}(mresults[i], variants[i])
		}

		lenresult := make(chan int)

		go func(b []byte) {
			for i := 0; i < len(substs); i++ {
				b = pcre.
					MustCompileJIT(substs[i].pattern, 0, pcre.STUDY_JIT_COMPILE).
					ReplaceAll(b, []byte(substs[i].replacement), 0)
			}
			lenresult <- len(b)
		}(b)

		for i := 0; i < len(variants); i++ {
			fmt.Printf("%s %d\n", variants[i], <-mresults[i])
		}

		fmt.Printf("\n%d\n%d\n%d\n", ilen, clen, <-lenresult)
	} else {
		panic(err)
	}
}
