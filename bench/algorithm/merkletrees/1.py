import sys


class Node(object):
    def __init__(self, value, left, right):
        self.value = value
        self.left = left
        self.right = right
        self.hash = None

    def check(self):
        if self.hash != None:
            if self.value != None:
                return True
            elif self.left and self.right:
                return self.left.check() and self.right.check()
        return False

    def cal_hash(self):
        if self.hash == None:
            if self.value != None:
                self.hash = self.value
            elif self.left and self.right:
                self.left.cal_hash()
                self.right.cal_hash()
                self.hash = self.left.get_hash() + self.right.get_hash()

    def get_hash(self):
        if self.hash == None:
            return -1
        else:
            return self.hash


def make(d):
    if d > 0:
        d -= 1
        return Node(None, make(d), make(d))
    return Node(1, None, None)


def main(n, min_depth=4):
    max_depth = max(min_depth + 2, n)
    stretch_depth = max_depth + 1
    stretch_tree = make(stretch_depth)
    stretch_tree.cal_hash()
    print(
        f'stretch tree of depth {stretch_depth}\t root hash: {stretch_tree.get_hash()} check: {stretch_tree.check().__str__().lower()}')

    long_lived_tree = make(max_depth)

    mmd = max_depth + min_depth
    for d in range(min_depth, stretch_depth, 2):
        iterations = 2 ** (mmd - d)
        sum = 0
        for _ in range(0, iterations):
            tree = make(d)
            tree.cal_hash()
            sum += tree.get_hash()
        print(f'{iterations}\t trees of depth {d}\t root hash sum: {sum}')

    long_lived_tree.cal_hash()
    print(
        f'long lived tree of depth {n}\t root hash: {long_lived_tree.get_hash()} check: {long_lived_tree.check().__str__().lower()}')


if __name__ == '__main__':
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 6
    main(n)
