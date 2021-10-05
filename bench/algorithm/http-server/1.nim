import os, strutils, json, strformat, random, lists
import asynchttpserver, asyncdispatch, httpclient

proc runServer(port: int) {.async.} =
  var server = newAsyncHttpServer()
  proc cb(req: Request) {.async.} =
    await req.respond(Http200, $parseJson(req.body)["value"].num)
  server.listen Port(port), "localhost"
  while true:
    if server.shouldAcceptRequest():
      await server.acceptRequest(cb)
    else:
      poll()

proc send(url:string, v:BiggestInt): Future[BiggestInt] {.async.} =
  let client = newAsyncHttpClient()
  defer: client.close()
  let payload = %*{
    "value": v
  }
  let body = $payload
  while true:
    try:
        var ret = await client.postContent(url, body = body)
        return parseInt(ret)
    except:
        discard

proc main(port: int) {.async.} =
    var n = 100
    if paramCount() > 0:
        n = parseInt(paramStr(1))
    let url = fmt"http://localhost:{port}/"
    var sum = BiggestInt(0)
    var tasks = initSinglyLinkedList[Future[BiggestInt]]()
    for i in 1 .. n:
        let t = send(url, i)
        var node = newSinglyLinkedNode[Future[BiggestInt]](t)
        tasks.append(node)
    for t in tasks:
        sum += await t
    stdout.writeLine(sum)

randomize()
let rand = initRand(1)
let port = rand(20000..40000)
asyncCheck runServer(port)
waitFor main(port)
