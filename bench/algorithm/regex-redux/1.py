# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# regex-dna program contributed by Dominique Wahli
# 2to3
# mp by Ahmad Syukri
# modified by Justin Peel
# converted from regex-dna program
# removed parallelization

import sys
from re import sub, findall
from datetime import datetime


seq = None

def init(arg):
    global seq
    seq = arg


def var_find(f):
    return len(findall(f, seq))


def main():
    file_name = '25000_in' if len(sys.argv) < 2 else sys.argv[1]
    with open(file_name, 'r') as fp:
        seq = fp.read()

    ilen = len(seq)

    seq = sub('>.*\n|\n', '', seq)
    clen = len(seq)
    init(seq)
    variants = (
        'agggtaaa|tttaccct',
        '[cgt]gggtaaa|tttaccc[acg]',
        'a[act]ggtaaa|tttacc[agt]t',
        'ag[act]gtaaa|tttac[agt]ct',
        'agg[act]taaa|ttta[agt]cct',
        'aggg[acg]aaa|ttt[cgt]ccct',
        'agggt[cgt]aa|tt[acg]accct',
        'agggta[cgt]a|t[acg]taccct',
        'agggtaa[cgt]|[acg]ttaccct')
    for v in variants:
        print(v, var_find(v))

    subst = {
        'tHa[Nt]': '<4>', 'aND|caN|Ha[DS]|WaS': '<3>', 'a[NSt]|BY': '<2>',
        '<[^>]*>': '|', '\\|[^|][^|]*\\|': '-'}
    for f, r in list(subst.items()):
        seq = sub(f, r, seq)

    print()
    print(ilen)
    print(clen)
    print(len(seq))


if __name__ == "__main__":
    with open("ready", "w") as f:
        f.write(str(round(datetime.utcnow().timestamp() * 1000)))
    main()
