import os, strutils, tables

const A:uint32=1103515245
const C:uint32=12345
const M:uint32=1 shl 31

proc lcg(seed: uint32): iterator (): uint32 =
  result = iterator (): uint32 =
    var x = seed
    while true:
      x = (A*x+C) mod M
      yield x

type
  LRU = object
    size: int
    tbl : OrderedTableRef[uint32, uint32]

method get(lru: LRU, key:uint32):(uint32,bool) =
  if lru.tbl.contains(key):
    let v = lru.tbl[key]
    lru.tbl.del(key)
    lru.tbl[key] = v
    (v, true)
  else:
    (uint32(0), false)

method put(lru: LRU, key:uint32, value:uint32) =
  if lru.tbl.contains(key):
    let v = lru.tbl[key]
    lru.tbl.del(key)
  elif lru.tbl.len() == lru.size:
    for k in lru.tbl.keys:
      lru.tbl.del(k)
      break
  lru.tbl[key] = value

let size =  if paramCount() > 0: parseInt(paramStr(1)) else: 100
let n =  if paramCount() > 1: parseInt(paramStr(2)) else: 100
let modular = uint32(size) * 10

let lru = LRU(size:size, tbl: newOrderedTable[uint32, uint32](size))
let rng0 = lcg(0)
let rng1 = lcg(1)

var hit = 0
var missed = 0

for i in 0..<n:
    let n0 = rng0() mod modular
    lru.put(n0, n0)
    let n1 = rng1() mod modular
    let (_, ok) = lru.get(n1)
    if ok:
      inc hit 
    else:
      inc missed
echo hit
echo missed
