## Coro-prime-sieve
## Author: Navid M

import std/[
  os,
  strutils
]

proc generate(limit: int): seq[int] =
  ## Generate a sequence of numbers starting from 2
  result = newSeq[int]()
  var i = 2
  while i <= limit:
    result.add(i)
    inc i

proc filter(numbers: seq[int], prime: int): seq[int] =
  ## Filter the numbers by removing multiples of the given prime
  result = newSeq[int]()
  for i in numbers:
    if i == prime or i mod prime != 0:
      result.add(i)

when isMainModule:
  let n = if paramCount() < 1: 100 else: parseInt(paramStr(1))
  var
    numbers = generate(int(high(int16)) * 3)
    primesFound = 0
    i = 0
  while primesFound < n:
    if i == numbers.len - 1:
      break
    let prime = numbers[i]
    echo prime
    inc primesFound
    numbers = filter(numbers, prime)
    inc i
