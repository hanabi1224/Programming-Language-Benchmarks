import sys
from collections import OrderedDict


class LRU:
    def __init__(self, size: int):
        self._dict = OrderedDict()
        self._size = size

    def get(self, key):
        if key in self._dict:
            self._dict.move_to_end(key, last=True)
        return self._dict.get(key, None)

    def put(self, key, value):
        if key in self._dict:
            self._dict.move_to_end(key, last=True)
        elif len(self._dict) == self._size:
            self._dict.popitem(last=False)
        self._dict[key] = value

    def _move_to_end_if_exists(self, key):
        if key in self._dict:
            self._dict.move_to_end(key, last=True)


def lcg(seed: int):
    A = 1103515245
    C = 12345
    M = 1 << 31
    while True:
        seed = (A*seed + C) % M
        yield seed


def main():
    size = int(sys.argv[1]) if len(sys.argv) > 1 else 100
    n = int(sys.argv[2]) if len(sys.argv) > 2 else 1000
    mod = size * 10
    rng0 = lcg(0)
    rng1 = lcg(1)
    hit = 0
    missed = 0
    lru = LRU(size)
    for i in range(0, n):
        n0 = next(rng0) % mod
        lru.put(n0, n0)
        n1 = next(rng1) % mod
        if lru.get(n1) == None:
            missed += 1
        else:
            hit += 1
    print(hit)
    print(missed)


if __name__ == '__main__':
    main()
