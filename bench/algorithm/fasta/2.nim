# Implementation of the fasta benchmark in Nim
# Based in parts on the rust version (1c.rs) by Rust Project Developers,
# TeXitoi and hanabi1224 as well as on the previous Nim version (1.nim)
# by hanabi1224 and jackhftang.

import std/[strutils, os]

const
  LINE_WIDTH = 60
  RAND_MOD = 139968
  ALU = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

proc to_offset_table[N](probs: array[N, (char, float64)]): array[N, (uint32, char)] =
  var offset = 0.0
  for it, (chr, prob) in probs:
    offset += prob
    result[it] = (uint32(offset * float64(RAND_MOD)), chr)

const
  IUB = to_offset_table({
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
    'Y': 0.02
  })
  HOMOSAPIENS = to_offset_table({
    'a': 0.3029549426680,
    'c': 0.1979883004921,
    'g': 0.1975473066391,
    't': 0.3015094502008
  })

type Rand = object
  seed: uint32

proc gen(rand: var Rand): uint32 {.inline.} =
  rand.seed = (rand.seed * 3877 + 29573) mod RAND_MOD
  result = rand.seed

proc gen(rand: var Rand, table: openArray[(uint32, char)]): char {.inline.} =
  let val = rand.gen()
  result = table[^1][1]
  for it in 0..<(table.len - 1):
    let (prob, chr) = table[it]
    if val <= prob:
      result = chr
      break

proc make_repeat_fasta(heading, source: static[string], n: int) =
  stdout.write(heading)
  var
    buffer = new_string(LINE_WIDTH + 1)
    offset = 0
  buffer[^1] = '\n'
  let
    count = n div source.len
    left = n mod source.len
  for it in 0..<count:
    for it2 in 0..<source.len: 
      buffer[offset] = source[it2]
      offset += 1
      if offset >= LINE_WIDTH:
        stdout.write(buffer)
        offset = 0
  for it in 0..<left:
    buffer[offset] = source[it]
    offset += 1
    if offset >= LINE_WIDTH:
      stdout.write(buffer)
      offset = 0
  if offset != 0:
    buffer[offset] = '\n'
    stdout.write(buffer[0..offset])

proc make_rand_fasta(heading: static[string],
                     table: openArray[(uint32, char)],
                     rand: var Rand,
                     n: int) =
  stdout.write(heading)
  var
    buffer = new_string(LINE_WIDTH + 1)
    offset = 0
  buffer[^1] = '\n'
  for it in 0..<n:
    buffer[offset] = rand.gen(table)
    offset += 1
    if offset >= LINE_WIDTH:
      stdout.write(buffer)
      offset = 0
  if offset != 0:
    buffer[offset] = '\n'
    stdout.write(buffer[0..offset])

proc main() =
  var
    rand = Rand(seed: 42)
    n = 1
  if param_count() > 0:
    n = parse_int(param_str(1))
  make_repeat_fasta(">ONE Homo sapiens alu\n", ALU, n * 2)
  make_rand_fasta(">TWO IUB ambiguity codes\n", IUB, rand, n * 3)
  make_rand_fasta(">THREE Homo sapiens frequency\n", HOMOSAPIENS, rand, n * 5)

when is_main_module:
  main()
