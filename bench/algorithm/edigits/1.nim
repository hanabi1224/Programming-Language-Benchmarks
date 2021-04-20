import os, strutils, bigints, math

const 
  LN10 = ln(10.0)
  LN_TAU = ln(TAU)
  ONE = initBigInt(1)
  TEN = initBigInt(10)

proc test_k(n : int64, k : int64):bool =  
  if k < 0:
    return false
  else:
    var ln_k_factorial = float64(k)*(ln(float64(k))-1.0) + 0.5*LN_TAU
    var log_10_k_factorial = ln_k_factorial / LN10
    return log_10_k_factorial >= float64(n+50)

proc binary_search(n:int64):int64 =
  var 
    a : int64 = 0
    b : int64 = 1
  while not test_k(n, b):
    a = b
    b *= 2
  while b - a > 1:
    let m : int64 = (a + b) div 2
    if test_k(n, m):
      b = m
    else:
      a = m
  return b

proc sum_terms(a:int64, b:int64) :(BigInt, BigInt) = 
  if b == a + 1:
    return (ONE, initBigInt(b))
  let mid = (a + b) div 2
  let (p_left, q_left) = sum_terms(a, mid)
  let (p_right, q_right) = sum_terms(mid, b)
  return (p_left * q_right + p_right, q_left * q_right)

var n =  27
if paramCount() > 0:
  n = parseInt(paramStr(1))
let k = binary_search(n)
var (p, q) = sum_terms(0, k-1)
p += q
var a = TEN.pow(initBigInt(n - 1))
let answer = p * a div q
let str = answer.toString(10)
var i = 0;
while i < n:
  if i+10 <= n:
    var sb = ""
    sb.add(str.substr(i, i+9))
    sb.add("\t:")
    sb.add((i+10).intToStr())
    stdout.writeLine(sb)
  else:
    var sb = ""
    sb.add(str.substr(i, n-1))
    var j = 0
    while j < 10 - (n mod 10):
      sb.add(" ")
      j += 1
    sb.add("\t:")
    sb.add(n.intToStr())
    stdout.writeLine(sb)
  i += 10
