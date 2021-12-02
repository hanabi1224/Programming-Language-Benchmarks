/* The Computer Language Benchmarks Game
* https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
*
* contributed by The Go Authors.
* Based on C program by Joern Inge Vestgaarden
* and Jorge Peixoto de Morais Neto.
* flag.Arg hack by Isaac Gouy
* parallel implementation by Chris Bainbridge
 */

package main

import (
	"bufio"
	"flag"
	"os"
	"runtime"
	"strconv"
	"sync"
)

var out *bufio.Writer

const WIDTH = 60   // Fold lines after WIDTH bytes
const LINES = 1024 // Lines to be processed at a time
const BLKLEN = WIDTH * LINES

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

type AminoAcid struct {
	p float64
	c byte
}

func AccumulateProbabilities(genelist []AminoAcid) {
	for i := 1; i < len(genelist); i++ {
		genelist[i].p += genelist[i-1].p
	}
	for i := 0; i < len(genelist); i++ {
		genelist[i].p *= IM
	}
}

// RepeatFasta prints the characters of the byte slice s. When it
// reaches the end of the slice, it goes back to the beginning.
// It stops after generating count characters.
// After each WIDTH characters it prints a newline.
// It assumes that WIDTH <= len(s) + 1.
func RepeatFasta(s []byte, count int) {
	pos := 0
	s2 := make([]byte, len(s)+WIDTH)
	copy(s2, s)
	copy(s2[len(s):], s)
	for count > 0 {
		line := min(WIDTH, count)
		out.Write(s2[pos : pos+line])
		out.WriteByte('\n')
		pos += line
		if pos >= len(s) {
			pos -= len(s)
		}
		count -= line
	}
}

var lastrandom uint32 = 42

const (
	IM = 139968
	IA = 3877
	IC = 29573
)

// Each element of genelist is a struct with a character and
// a floating point number p between 0 and 1.
// RandomFasta generates a random float r and
// finds the first element such that p >= r.
// This is a weighted random selection.
// RandomFasta then prints the character of the array element.
// This sequence is repeated count times.
// Between each WIDTH consecutive characters, the function prints a newline.
func RandomFasta(genelist []AminoAcid, count int) {
	var rng sync.Mutex
	threads := runtime.NumCPU()

	c0 := make(chan bool, 1)
	c0 <- true
	c1 := make(chan bool, 1)
	cs := make(chan bool)
	for thread := 0; thread < threads; thread++ {
		go func() {
			var block [BLKLEN]uint32
			buf := make([]byte, BLKLEN+LINES)
			for {
				rng.Lock()
				if count == 0 {
					rng.Unlock()
					break
				}
				line := min(BLKLEN, count)
				count -= line
				// generate random number block
				for pos := 0; pos < line; pos++ {
					lastrandom = (lastrandom*IA + IC) % IM
					block[pos] = lastrandom
				}
				c0x := c0
				c1x := c1
				countx := count
				c0 = c1
				c1 = make(chan bool, 1)
				rng.Unlock()
				// convert random block to aminoacid block
				j := 0
				for i := 0; i < line; i++ {
					if i > 0 && i%WIDTH == 0 {
						buf[j] = '\n'
						j++
					}
					r := float64(int(block[i]))
					for _, v := range genelist {
						if v.p >= r {
							buf[j] = v.c
							j++
							break
						}
					}
				}
				// fix len of buf (final block is shorter)
				buf[j] = '\n'
				j++
				if len(buf) != j {
					buf = buf[:j]
				}
				// print buf
				<-c0x
				out.Write(buf)
				if countx != 0 {
					c1x <- true
				} else {
					cs <- true
				}
			}
		}()
	}
	<-cs
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	out = bufio.NewWriter(os.Stdout)
	defer out.Flush()

	flag.Parse()
	n := 0
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}

	iub := []AminoAcid{
		{0.27, 'a'},
		{0.12, 'c'},
		{0.12, 'g'},
		{0.27, 't'},
		{0.02, 'B'},
		{0.02, 'D'},
		{0.02, 'H'},
		{0.02, 'K'},
		{0.02, 'M'},
		{0.02, 'N'},
		{0.02, 'R'},
		{0.02, 'S'},
		{0.02, 'V'},
		{0.02, 'W'},
		{0.02, 'Y'},
	}

	homosapiens := []AminoAcid{
		{0.3029549426680, 'a'},
		{0.1979883004921, 'c'},
		{0.1975473066391, 'g'},
		{0.3015094502008, 't'},
	}

	AccumulateProbabilities(iub)
	AccumulateProbabilities(homosapiens)

	alu := []byte(
		"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
			"GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
			"CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
			"ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
			"GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
			"AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
			"AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")

	out.WriteString(">ONE Homo sapiens alu\n")
	RepeatFasta(alu, 2*n)
	out.WriteString(">TWO IUB ambiguity codes\n")
	RandomFasta(iub, 3*n)
	out.WriteString(">THREE Homo sapiens frequency\n")
	RandomFasta(homosapiens, 5*n)
}
