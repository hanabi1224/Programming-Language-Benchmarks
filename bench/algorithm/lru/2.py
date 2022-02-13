import sys
from collections import deque


class LRU:
    def __init__(self, size: int):
        self._size = size
        self._key_lookup = {}
        self._entries = deque(maxlen=size)
        self._idx_offset = 0

    def get(self, key):
        idx = self._key_lookup.get(key, None)
        if idx == None:
            return None
        idx -= self._idx_offset
        k, v = self._entries[idx]
        self._move_to_end(idx, (k, v))
        return v

    def put(self, key, value):
        idx = self._key_lookup.get(key, None)
        if idx == None:
            if len(self._entries) == self._size:
                self._pop_front()
            self._key_lookup[key] = len(self._entries) + self._idx_offset
            self._entries.append((key, value))
        else:
            idx -= self._idx_offset
            k, v = self._entries[idx]
            self._move_to_end(idx, (k, v))
            self._entries[-1] = (k, value)

    def _pop_front(self):
        k, v = self._entries.popleft()
        del self._key_lookup[k]
        if self._idx_offset < 10000:
            self._idx_offset += 1
        else:
            self._idx_offset = 0
            for i in range(0, len(self._entries)):
                k, v = self._entries[i]
                self._key_lookup[k] = i

    def _move_to_end(self, idx, pair):
        if idx + 1 < len(self._entries):
            self._entries.remove(pair)
            self._entries.append(pair)
            for i in range(idx, len(self._entries)):
                k, v = self._entries[i]
                self._key_lookup[k] = i + self._idx_offset


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
