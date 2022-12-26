import sys


def nsieve(n):
    count = 0
    flags = [True] * n
    for i in range(2, n):
        if i * i >= n:
            count += len(list(filter(lambda b: b, flags[i:])))
            break
        if flags[i]:
            count += 1
            for j in range(i << 1, n, i):
                flags[j] = False
    print(f'Primes up to {n:8} {count:8}')


if __name__ == '__main__':
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 4
    for i in range(0, 3):
        nsieve(10000 << (n-i))
