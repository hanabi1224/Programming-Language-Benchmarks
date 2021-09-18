import math, os, strutils, strformat, md5

proc add(a: array[8, float64], b: array[8, float64], r: var array[8, float64]):auto=
  for i in 0..<8:
    r[i] = a[i] + b[i]

proc minus(a: array[8, float64], b: array[8, float64], r: var array[8, float64]):auto=
  for i in 0..<8:
    r[i] = a[i] - b[i]

proc mul(a: array[8, float64], b: array[8, float64], r: var array[8, float64]):auto=
  for i in 0..<8:
    r[i] = a[i] * b[i]

proc mbrot8(cr: array[8, float64], civ:float64):auto=
  var ci: array[8, float64]
  for i in 0..<8:
    ci[i] = civ
  var zr: array[8, float64]
  var zi: array[8, float64]
  var tr: array[8, float64]
  var ti: array[8, float64]
  var absz: array[8, float64]
  for _ in 0..<10:
    for _ in 0..<5:
      var tmp: array[8, float64]
      add(zr, zr, tmp)
      mul(tmp, zi, tmp)
      add(tmp, ci, zi)

      minus(tr, ti, tmp)
      add(tmp, cr, zr)

      mul(zr, zr, tr)
      mul(zi, zi, ti)
    add(tr, ti, absz)
    var terminate = true
    for i in 0..<8:
      if absz[i] <= 4.0:
        terminate = false
        break
    if terminate:
      return byte(0)
  var accu = byte(0)
  for i in 0..<8:
    if absz[i] <= 4.0:
      accu = accu or byte(0x80 shr i)
    else:
      accu = accu or byte(0)
  accu

var pCount = paramCount()
var n = 1
if pCount > 0:
  n = parseInt(paramStr(1))
n = (n+7) div 8 * 8

echo &"P4\n{n} {n}"

let chunk_size = n div 8
let inv = 2.0 / float64(n)

var xloc = newSeq[array[8, float64]](chunk_size)
for i in 0..<n:
  xloc[i div 8][i mod 8] = float64(i) * inv - 1.5

var rows = newString(n * chunk_size)
for chunk_id in 0..<n:
  let ci = float64(chunk_id) * inv - 1.0
  for i in 0..<chunk_size:
    let r = mbrot8(xloc[i], ci)
    if r > 0:
      rows[chunk_id * chunk_size + i] = char(r)

echo getMD5(rows)
