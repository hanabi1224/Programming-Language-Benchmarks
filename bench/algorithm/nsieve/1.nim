import os, strutils, strformat

proc nsieve(n: int): auto =
  var count = 0
  var flags = newSeq[int8](n)
  for i in 2..<n:
    if flags[i] == 0:
      count += 1
      for j in countup(i*2, n, i):
        flags[j] = 1
  echo fmt"Primes up to {n:8} {count:8}"

var pCount = paramCount()
var n = 10
if pCount > 0:
  n = parseInt(paramStr(1))

for i in 0..<3:
  nsieve(10000 shl (n-i))
