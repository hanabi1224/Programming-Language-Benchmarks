import os, strutils, strformat, threadpool

const CHANNEL_SIZE = 1

proc generate(chan: ptr Channel[int]) =
  var i = 2
  while true:
    chan[].send(i)
    i += 1

proc filter(chan_in: ptr Channel[int], n: int) =
  let prime = chan_in[].recv()
  echo prime
  if n < 2:
    quit(0)
  let chan_out = cast[ptr Channel[int]](
    allocShared0(sizeof(Channel[int]))
  )
  chan_out[].open(CHANNEL_SIZE)
  spawn(filter(chan_out, n-1))
  while true:
    let i = chan_in[].recv()
    if i mod prime != 0:
      chan_out[].send(i)

proc find_primes(n: int) =
  var ch = cast[ptr Channel[int]](
    allocShared0(sizeof(Channel[int]))
  )
  ch[].open(CHANNEL_SIZE)
  spawn(generate(ch))
  spawn(filter(ch, n))
  sync()

var n = 100
if paramCount() > 0:
  n = parseInt(paramStr(1))

find_primes(n)
