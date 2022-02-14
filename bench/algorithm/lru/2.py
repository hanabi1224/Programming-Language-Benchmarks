import sys


class LinkedListNode(object):
    def __init__(self, data):
        self.data = data
        self.prev = None
        self.next = None


class LinkedList(object):
    def __init__(self):
        self.head = None
        self.tail = None
        self.len = 0

    def add(self, data):
        node = LinkedListNode(data)
        self.__add_node(node)
        self.len += 1
        return node

    def __add_node(self, node):
        if self.head is None:
            self.head = node
            node.prev = None
        elif self.tail:
            node.prev = self.tail
            self.tail.next = node
        self.tail = node
        node.next = None

    def __remove(self, node):
        if self.head == node:
            self.head = node.next
        if self.tail == node:
            self.tail = node.prev
        if node.prev:
            node.prev.next = node.next
        if node.next:
            node.next.prev = node.prev

    def move_to_end(self, node):
        self.__remove(node)
        self.__add_node(node)

    def pop_head(self):
        if self.head:
            head = self.head
            self.head = head.next
            self.len -= 1
            return head
        else:
            return None


class LRU:
    def __init__(self, size: int):
        self._size = size
        self._key_lookup = {}
        self._entries = LinkedList()

    def get(self, key):
        node = self._key_lookup.get(key, None)
        if node == None:
            return None
        self._entries.move_to_end(node)
        return node.data[1]

    def put(self, key, value):
        node = self._key_lookup.get(key, None)
        if node != None:
            node.data = (key, value)
            self._entries.move_to_end(node)
            return
        elif self._entries.len == self._size:
            head = self._entries.pop_head()
            del self._key_lookup[head.data[0]]
        self._key_lookup[key] = self._entries.add((key, value))


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
