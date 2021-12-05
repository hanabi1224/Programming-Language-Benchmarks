/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Mark van Weert
 * based on Go#6 C++#2
 */

package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"runtime"
	"sort"
	"strings"
	"sync"
)

var toNum = strings.NewReplacer("A", string(0), "C", string(1), "T", string(2), "G", string(3))
var toChar = []byte{'A', 'C', 'T', 'G'}

func main() {
	dna := input()

	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	fmt.Fprintln(out, WriteFrequencies(dna, 1))
	fmt.Fprintln(out, WriteFrequencies(dna, 2))
	fmt.Fprintln(out, WriteCount(dna, "GGT"))
	fmt.Fprintln(out, WriteCount(dna, "GGTA"))
	fmt.Fprintln(out, WriteCount(dna, "GGTATT"))
	fmt.Fprintln(out, WriteCount(dna, "GGTATTTTAATT"))
	fmt.Fprintln(out, WriteCount(dna, "GGTATTTTAATTTATAGT"))
}

// input returns the transformed data
func input() []byte {
	fileName := "25000_in"
	if len(os.Args) > 1 {
		fileName = os.Args[1]
	}
	if f, err := os.Open(fileName); err == nil {
		defer f.Close()
		scanner := bufio.NewScanner(f)
		data := readStdin(scanner)
		// A = 0, C = 1, T = 2, G = 3
		for idx := range data {
			data[idx] = data[idx] >> 1 & 3
		}
		return data
	} else {
		panic(err)
	}
}

// readStdin returns the data marked with >THREE from stdin
func readStdin(scanner *bufio.Scanner) []byte {
	var lineCount int
	for scanner.Scan() {
		// Keep reading until we encounter the start marker >THREE
		line := scanner.Text()
		if len(line) == 0 {
			continue
		}
		lineCount++
		if line[0] == '>' && strings.HasPrefix(string(line), ">THREE") {
			data := make([]byte, 0, lineCount*61)
			for scanner.Scan() {
				line := scanner.Text()
				if len(line) > 0 {
					data = append(data, line...)
				}
			}
			return data
		}
	}
	return []byte{}
}

// WriteFrequencies returns the frequencies of the nucleotides with the given length
func WriteFrequencies(data []byte, size int) string {
	// Get counts
	counts := startCount32(data, size)

	// Store the map keys and values in a struct to sort them
	type kv struct {
		Key   uint32
		Value int
	}
	var sortedCounts []kv
	for k, v := range counts {
		sortedCounts = append(sortedCounts, kv{k, *v})
	}

	sort.Slice(sortedCounts, func(i, j int) bool {
		return sortedCounts[i].Value > sortedCounts[j].Value
	})

	sum := float32(len(data) - size + 1)
	var result string
	for _, count := range sortedCounts {
		result += fmt.Sprintf("%v %.3f\n", decompress32(count.Key, size), 100.0*float32(count.Value)/sum)
	}

	return result
}

// WriteCount returns the number of given nucleotides in the data
func WriteCount(data []byte, nucleotide string) string {
	size := len(nucleotide)

	if len(nucleotide) <= 16 {
		// Get counts nucleotide key can fit in an uint32
		counts := startCount32(data, size)

		key := compress32([]byte(toNum.Replace(nucleotide)))

		count := counts[key]
		if count == nil {
			count = new(int)
		}

		return fmt.Sprintf("%v\t%v", *count, nucleotide)

	} else {
		// Get counts for longer nucleotides
		counts := startCount64(data, size)
		key := compress64([]byte(toNum.Replace(nucleotide)))
		count := counts[key]
		if count == nil {
			count = new(int)
		}

		return fmt.Sprintf("%v\t%v", *count, nucleotide)
	}
}

func startCount32(data []byte, size int) map[uint32]*int {
	maps := make([]map[uint32]*int, runtime.NumCPU())

	wg := sync.WaitGroup{}

	// Create a map for each goroutine we will spawn
	for i := 0; i < len(maps); i++ {
		maps[i] = make(map[uint32]*int)
		wg.Add(1)
		go calc32(data, maps[i], i, size, &wg)
	}

	wg.Wait()

	// Add counts from all goroutines to the first map
	m0 := maps[0]
	for i := 1; i < len(maps); i++ {
		for i2, val := range maps[i] {
			if val == nil {
				continue
			}
			if m0[i2] == nil {
				m0[i2] = new(int)
			}
			*m0[i2] += *val
		}
	}

	return m0
}

func startCount64(data []byte, size int) map[uint64]*int {
	maps := make([]map[uint64]*int, runtime.NumCPU())

	wg := sync.WaitGroup{}

	// Create a map for each goroutine we will spawn
	for i := 0; i < len(maps); i++ {
		maps[i] = make(map[uint64]*int)
		wg.Add(1)
		go calc64(data, maps[i], i, size, &wg)
	}

	wg.Wait()

	// Add counts from all goroutines to the first map
	m0 := maps[0]
	for i := 1; i < len(maps); i++ {
		for i2, val := range maps[i] {
			if val == nil {
				continue
			}
			if m0[i2] == nil {
				m0[i2] = new(int)
			}
			*m0[i2] += *val
		}
	}

	return m0
}

func calc32(data []byte, result map[uint32]*int, begin int, size int, wg *sync.WaitGroup) {
	var key uint32
	goroutineCount := uint(runtime.NumCPU())

	// Init key
	for i := 0; i < size; i++ {
		key <<= 2
		key |= uint32(data[i+begin])
	}

	// Create a map to do the counts in.
	// We don't use the map we are passed but instead return a copy of this

	p := new(int)
	*p++
	result[key] = p

	nsize := uint(size)
	if goroutineCount < nsize {
		nsize = goroutineCount
	}

	mask := ^uint32(math.MaxUint32 << uint(2*size))

	start := uint(begin) + goroutineCount
	end := uint(len(data) + 1 - size)
	for idx := start; idx < end; idx += goroutineCount {
		// Update the key with 1 byte at a time
		for i := uint(0); i < nsize; i++ {
			key <<= 2
			key |= uint32(data[idx+i])
		}

		// Mask out excess information
		key &= mask

		// Get pointer to the count for this key from the map
		// For a low number of different keys using a map with
		// pointers is faster than just using a value map and do: m[key]++
		p, ok := result[key]
		if !ok {
			p = new(int)
			result[key] = p
		}
		*p++
	}

	// Signal done
	wg.Done()
}

func calc64(data []byte, result map[uint64]*int, begin int, size int, wg *sync.WaitGroup) {
	var key uint64
	goroutineCount := uint(runtime.NumCPU())

	// Init key
	for i := 0; i < size; i++ {
		key <<= 2
		key |= uint64(data[i+begin])
	}

	// Create a map to do the counts in. We don't use the map
	// we are passed but instead return a copy of this

	p := new(int)
	*p++
	result[key] = p

	nsize := uint(size)
	if goroutineCount < nsize {
		nsize = goroutineCount
	}

	mask := ^uint64(math.MaxUint64 << uint(2*size))

	start := uint(begin) + goroutineCount
	end := uint(len(data) + 1 - size)
	for idx := start; idx < end; idx += goroutineCount {
		// Update the key with 1 byte at a time
		if uint(len(data)) < idx+nsize {
			continue
		}
		for i := uint(0); i < nsize; i++ {
			key <<= 2
			key |= uint64(data[idx+i])
		}

		// Mask out excess information
		key &= mask

		// Get pointer to the count for this key from the map
		// For a low number of different keys using a map with
		// pointers is faster than just using a value map and do: m[key]++
		p, ok := result[key]
		if !ok {
			p = new(int)
			result[key] = p
		}
		*p++
	}

	// Signal done
	wg.Done()

}

func compress64(sequence []byte) uint64 {
	var num uint64
	for _, char := range sequence {
		num = num<<2 | uint64(char)
	}
	return num
}

func compress32(sequence []byte) uint32 {
	var num uint32
	for _, char := range sequence {
		num = num<<2 | uint32(char)
	}
	return num
}

func decompress32(num uint32, length int) string {
	var sequence = make([]byte, length)
	for i := 0; i < length; i++ {
		sequence[length-i-1] = toChar[byte(num&3)]
		num = num >> 2
	}
	return string(sequence)
}
