import os, strutils, strformat, bitarray

proc nsieve(n: int): auto =
  var count = 0
  var flags = newBitsArray(n)
  for i in 2..<n:
    if not flags.testBit(i):
      count += 1
      for j in countup(i*2, n, i):
        flags.setBit(j)
  echo fmt"Primes up to {n:8} {count:8}"

var pCount = paramCount()
var n = 10
if pCount > 0:
  n = parseInt(paramStr(1))

for i in 0..<3:
  nsieve(10000 shl (n-i))
