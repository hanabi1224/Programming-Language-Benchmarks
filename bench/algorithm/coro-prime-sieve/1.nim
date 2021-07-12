import os, strutils, coro

const CHANNEL_SIZE = 1

proc generate(chan:ptr Channel[int]) = 
  var i = 2
  while true:    
    chan[].send(i)
    stdout.write("gen:")
    stdout.writeLine(i)
    i += 1

proc filter(chan_in:ptr Channel[int], n:int) =
  stdout.write("chan:")
  stdout.writeLine(n)
  if n < 2:
    return
  let prime = chan_in[].recv()
  stdout.writeLine(prime)
  let chan_out = cast[ptr Channel[int]](
    allocShared0(sizeof(Channel[int]))
  )
  chan_out[].open(CHANNEL_SIZE)
  start(proc = filter(chan_out, n-1))
  run()
  while true:
    let i = chan_in[].recv()
    stdout.writeLine(i)
    if i mod prime != 0:
      chan_out[].send(i)

proc find_primes(n:int) =
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
