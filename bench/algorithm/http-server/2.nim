import os, strutils, json, strformat, random, lists
import asyncdispatch, httpclient
import httpbeast, options, threadpool

proc onRequest(req: Request) {.async.} =
  if req.httpMethod == some(HttpPost):
    case req.path.get()
    of "/":
      let n = parseJson(req.body.get())["value"].num
      req.send(Http200, fmt"{n}")
    else:
      req.send(Http404)

proc runServer(port: int) =
  httpbeast.run onRequest, initSettings(Port(port), "localhost")

proc send(url: string, v: BiggestInt): Future[BiggestInt] {.async.} =
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
spawn runServer port
waitFor main(port)
