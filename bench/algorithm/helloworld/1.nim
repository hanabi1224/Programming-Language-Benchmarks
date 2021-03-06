import os

when isMainModule:
  var n = ""
  if paramCount() > 0:
    n = paramStr(1)
  echo "Hello world ", n, "!"
