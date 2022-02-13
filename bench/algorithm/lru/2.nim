import os, strutils, lists, tables, options

const A: uint32 = 1103515245
const C: uint32 = 12345
const M: uint32 = 1 shl 31

# LruCache impl from https://github.com/jackhftang/lrucache.nim/blob/master/src/lrucache.nim
# with some modifications
type
  Node[K, T] = object
    key: K
    val: T

  LruCache*[K, T] = ref object
    capacity: int
    list: DoublyLinkedList[Node[K, T]]
    table: Table[K, DoublyLinkedNode[Node[K, T]]]

proc newLruCache*[K, T](capacity: int): LruCache[K, T] =
  ## Create a new Least-Recently-Used (LRU) cache that store the last `capacity`-accessed items.
  LruCache[K, T](
    capacity: capacity,
    list: initDoublyLinkedList[Node[K, T]](),
    table: initTable[K, DoublyLinkedNode[Node[K, T]]](capacity)
  )

proc len*[K, T](cache: LruCache[K, T]): int =
  ## Return number of key in cache
  cache.table.len

proc resize[K, T](cache: LruCache[K, T]) =
  while cache.len > cache.capacity:
    let t = cache.list.tail
    cache.table.del(t.value.key)
    cache.list.remove t

proc addNewNode[K, T](cache: LruCache[K, T], key: K, val: T) =
  # create new node
  let node = newDoublyLinkedNode[Node[K, T]](
    Node[K, T](key: key, val: val)
  )
  # put on table and prepend new node
  cache.table[key] = node
  cache.list.prepend node
  # remove old node if exceed capacity
  cache.resize()

proc `[]`*[K, T](cache: LruCache[K, T], key: K): Option[T] =
  var node = cache.table.getOrDefault(key, nil)
  if node.isNil:
    result = none(T)
  else:
    result = some(node.value.val)
    cache.list.remove node
    cache.list.prepend node

proc `[]=`*[K, T](cache: LruCache[K, T], key: K, val: T) =
  ## Put value `v` in cache with key `k`.
  ## Remove least recently used value from cache if length exceeds capacity.

  # read current node
  var node = cache.table.getOrDefault(key, nil)
  if node.isNil:
    cache.addNewNode(key, val)
  else:
    # set value
    node.value.val = val
    # move to head
    cache.list.remove node
    cache.list.prepend node

proc lcg(seed: uint32): iterator (): uint32 =
  result = iterator (): uint32 =
    var x = seed
    while true:
      x = (A*x+C) mod M
      yield x

let size = if paramCount() > 0: parseInt(paramStr(1)) else: 100
let n = if paramCount() > 1: parseInt(paramStr(2)) else: 100
let modular = uint32(size) * 10

let lru = newLRUCache[uint32, uint32](size)
let rng0 = lcg(0)
let rng1 = lcg(1)

var hit = 0
var missed = 0

for i in 0..<n:
  let n0 = rng0() mod modular
  lru[n0] = n0
  let n1 = rng1() mod modular
  if lru[n1].isSome:
    inc hit
  else:
    inc missed
echo hit
echo missed
