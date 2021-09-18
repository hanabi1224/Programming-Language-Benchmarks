import math, os, strutils, strformat, threadpool
{.experimental: "parallel".}

type
  FloatArrayRef = ref seq[float64]

proc `=deepCopy`(x: FloatArrayRef): FloatArrayRef =
  x

proc evala(i:int,j:int):auto =
  var sum = i + j
  float64(sum * (sum + 1) div 2 + i + 1)

proc times_i(v: FloatArrayRef, u: FloatArrayRef, i: int, n: int, reverse:bool):auto=
  var sum = 0.0
  for j in 0..<n:
    if reverse:
      sum += u[j] / evala(j, i)
    else:
      sum += u[j] / evala(i, j)
  v[i] = sum

proc times(v: FloatArrayRef, u: FloatArrayRef, n: int, reverse:bool):auto=
  parallel:
    for i in 0..<n:
      spawn times_i(v, u, i, n, reverse)

proc a_times_transp(v: FloatArrayRef, u: FloatArrayRef, n: int): auto =
  var x: FloatArrayRef
  new(x)
  x[] = newSeq[float64](n)
  times(x, u, n, false)
  times(v, x, n, true)

var pCount = paramCount()
var n = 100
if pCount > 0:
  n = parseInt(paramStr(1))

var u: FloatArrayRef
new(u)
u[] = newSeq[float64](n)
var v: FloatArrayRef
new(v)
v[] = newSeq[float64](n)
for i in 0..<n:
  u[i] = 1.0
  v[i] = 1.0
for _ in 0..<10:
  a_times_transp(v, u, n)
  a_times_transp(u, v, n)
var vbv = 0.0
var vv = 0.0
for i in 0..<n:
  vbv += v[i] * u[i]
  vv += pow(v[i], 2)
let ans = sqrt(vbv / vv)
echo fmt"{ans:.9f}"
