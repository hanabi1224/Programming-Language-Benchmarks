package main

/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Contributed by Dean Becker
*/

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
)

// regex and replacement string
type subst struct {
	re                *regexp.Regexp
	replacementString []byte
}

// regex and result value
type variant struct {
	re     *regexp.Regexp
	result int // for storing the result of the re
}

var (
	variants      []*variant
	substitutions []*subst
	bytes         []byte
	originalLen   int
	cleanedLen    int
	cleanRE       *subst
)

func main() {

	doneCh := make(chan int)

	// initialize concurrently
	go loadFile(doneCh)
	go initRegexes(doneCh)

	// wait for the above routines to finish
	<-doneCh
	<-doneCh

	// clean the input
	bytes = cleanRE.re.ReplaceAllLiteral(bytes, cleanRE.replacementString)
	cleanedLen = len(bytes)

	// since this one takes longest, start it first
	finalLen := make(chan int)
	go func() {
		// copy our bytes so we don't trounce the variant routines
		bb := make([]byte, len(bytes))
		copy(bb, bytes)

		for _, sub := range substitutions {
			bb = sub.re.ReplaceAll(bb, sub.replacementString)
		}

		finalLen <- len(bb)
	}()

	// variant routines
	for _, v := range variants {
		go countVariants(doneCh, v)
	}

	// await all variant results (so we can see them in order)
	for range variants {
		<-doneCh
	}

	// print all variant results
	for _, v := range variants {
		fmt.Printf("%s %d\n", v.re.String(), v.result)
	}

	// print finalLen when it's available
	fmt.Printf("\n%d\n%d\n%d\n", originalLen, cleanedLen, <-finalLen)

}

func loadFile(doneCh chan int) {
	fileName := "25000_in"
	if len(os.Args) > 1 {
		fileName = os.Args[1]
	}
	if f, err := os.Open(fileName); err == nil {
		defer f.Close()

		var err error
		bytes, err = ioutil.ReadAll(f)
		if err != nil {
			fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
			os.Exit(2)
		}
		originalLen = len(bytes)
		doneCh <- 1
	} else {
		panic(err)
	}
}

func countVariants(doneCh chan int, v *variant) {
	v.result = len(v.re.FindAll(bytes, -1))
	doneCh <- 1
}

func initRegexes(doneCh chan int) {

	variants = []*variant{
		{re: regexp.MustCompile("agggtaaa|tttaccct")},
		{re: regexp.MustCompile("[cgt]gggtaaa|tttaccc[acg]")},
		{re: regexp.MustCompile("a[act]ggtaaa|tttacc[agt]t")},
		{re: regexp.MustCompile("ag[act]gtaaa|tttac[agt]ct")},
		{re: regexp.MustCompile("agg[act]taaa|ttta[agt]cct")},
		{re: regexp.MustCompile("aggg[acg]aaa|ttt[cgt]ccct")},
		{re: regexp.MustCompile("agggt[cgt]aa|tt[acg]accct")},
		{re: regexp.MustCompile("agggta[cgt]a|t[acg]taccct")},
		{re: regexp.MustCompile("agggtaa[cgt]|[acg]ttaccct")},
	}

	substitutions = []*subst{
		{regexp.MustCompile("tHa[Nt]"), []byte("<4>")},
		{regexp.MustCompile("aND|caN|Ha[DS]|WaS"), []byte("<3>")},
		{regexp.MustCompile("a[NSt]|BY"), []byte("<2>")},
		{regexp.MustCompile("<[^>]*>"), []byte("|")},
		{regexp.MustCompile("\\|[^|][^|]*\\|"), []byte("-")},
	}

	cleanRE = &subst{regexp.MustCompile("(>[^\n]+)?\n"), []byte("")}

	doneCh <- 1
}
