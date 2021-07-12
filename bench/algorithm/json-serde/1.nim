import os, strutils, json, md5, strformat

var pCount = paramCount()
var fileName = "sample"
if pCount > 0:
  fileName = paramStr(1)
var n = 3
if pCount > 1:
  n = parseInt(paramStr(2))

# var jsonStr = readFile("$#.json" % [fileName])
var jsonStr = readFile(fmt"{fileName}.json")
var i = 1
while i <= n:
  var data = parseJson(jsonStr)
  var prettified = pretty(data, i)
  # echo prettified
  echo getMD5(prettified)
  i += 1
