import os, strutils, strformat, coro

const CHANNEL_SIZE = 1

proc generate(chan: ptr Channel[int]) =
  var i = 2
  while true:
    if not chan[].trySend(i):
      suspend()
      continue
    suspend()
    i += 1

proc filter(chan_in: ptr Channel[int], n: int) =
  var
    ok: bool
    prime: int
  (ok, prime) = chan_in[].tryRecv()
  while not ok:
    suspend()
    (ok, prime) = chan_in[].tryRecv()
  echo prime
  if n < 2:
    quit(0)
  suspend()
  let chan_out = cast[ptr Channel[int]](
    allocShared0(sizeof(Channel[int]))
  )
  chan_out[].open(CHANNEL_SIZE)
  start(proc = filter(chan_out, n-1))
  while true:
    var i: int
    (ok, i) = chan_in[].tryRecv()
    while not ok:
      suspend()
      (ok, i) = chan_in[].tryRecv()
    if i mod prime != 0:
      ok = chan_out[].trySend(i)
      while not ok:
        suspend()
        ok = chan_out[].trySend(i)
      suspend()
    else:
      suspend()

proc find_primes(n: int) =
  var ch = cast[ptr Channel[int]](
    allocShared0(sizeof(Channel[int]))
  )
  ch[].open(CHANNEL_SIZE)
  start(proc = generate(ch))
  start(proc = filter(ch, n))
  run()

var n = 100
if paramCount() > 0:
  n = parseInt(paramStr(1))

find_primes(n)
