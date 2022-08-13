import os, re, strutils, strformat

var pCount = paramCount()
var fileName = "25000_in"
if pCount > 0:
  fileName = paramStr(1)
var content = readFile(fileName)
var ilen = content.len
content = replace(content, re">.*\n|\n", "")
var clen = content.len
for it, p in [
  "agggtaaa|tttaccct",
  "[cgt]gggtaaa|tttaccc[acg]",
  "a[act]ggtaaa|tttacc[agt]t",
  "ag[act]gtaaa|tttac[agt]ct",
  "agg[act]taaa|ttta[agt]cct",
  "aggg[acg]aaa|ttt[cgt]ccct",
  "agggt[cgt]aa|tt[acg]accct",
  "agggta[cgt]a|t[acg]taccct",
  "agggtaa[cgt]|[acg]ttaccct",
  ]:
  var count = findAll(content, re(p)).len
  echo &"{p} {count}"
for it, (p, r) in [
  (re"tHa[Nt]", "<4>"),
  (re"aND|caN|Ha[DS]|WaS", "<3>"),
  (re"a[NSt]|BY", "<2>"),
  (re"<[^>]*>", "|"),
  (re"\|[^|][^|]*\|", "-"),
]:
  content = replace(content, p, r)
echo &"\n{ilen}\n{clen}\n{content.len}"
