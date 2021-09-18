import math, os, strutils, strformat, md5, tables

const line_width = 60
const im = 139968
const ia = 3877
const ic = 29573
const alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"
type
  RandGen = object
    seed: int
var randGen = RandGen(seed:42)
var iub = {
  'a': 0.27,
  'c': 0.12,
  'g': 0.12,
  't': 0.27,
  'B': 0.02,
  'D': 0.02,
  'H': 0.02,
  'K': 0.02,
  'M': 0.02,
  'N': 0.02,
  'R': 0.02,
  'S': 0.02,
  'V': 0.02,
  'W': 0.02,
  'Y': 0.02,
}.toOrderedTable
var homosapiens = {
  'a': 0.3029549426680,
  'c': 0.1979883004921,
  'g': 0.1975473066391,
  't': 0.3015094502008,
}.toOrderedTable

proc nextRand():auto=
  randGen.seed = (randGen.seed * ia + ic) mod im
  float64(randGen.seed) / float64(im)

proc make_repeat_fasta(id:string, desc:string, src:string, n: int):auto = 
  echo &">{id} {desc}"
  var char_print_idx = 0
  var sb = newString(line_width)
  for _ in 0..(n div src.len):
    for c in src:
      sb[char_print_idx mod line_width] = c
      char_print_idx += 1
      if char_print_idx mod line_width == 0:
        echo sb
      if char_print_idx >= n:
        let rmd_len = char_print_idx mod line_width
        if rmd_len > 0:
          echo sb[0..<rmd_len]
        break

proc make_random_fasta(id:string, desc:string, table: var OrderedTable[char, float64], n: int): auto=
  echo &">{id} {desc}"
  var prob = 0.0
  for k, p in table:
    prob += p
    table[k] = prob
  
  var n_char_printed = 0
  var sb = newString(line_width)
  for _ in 0..<n:
    let rand = nextRand()
    for k, p in table:
      if p > rand:
        sb[n_char_printed mod line_width] = k
        n_char_printed += 1
        if n_char_printed mod line_width == 0:
          echo sb
        break
  if n_char_printed >= n:
    let rmd_len = n_char_printed mod line_width
    if rmd_len > 0:
      echo sb[0..<rmd_len]
    return

proc main():auto=
  var pCount = paramCount()
  var n = 1
  if pCount > 0:
    n = parseInt(paramStr(1))
  make_repeat_fasta("ONE", "Homo sapiens alu", alu, n * 2)
  make_random_fasta("TWO", "IUB ambiguity codes", iub, n * 3)
  make_random_fasta("THREE", "Homo sapiens frequency", homosapiens, n * 5)
main()
