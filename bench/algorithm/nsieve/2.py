import sys


def nsieve(n):
    count = 0
    flags = [True] * n
    for i in range(2, n):
        if flags[i]:
            count += 1
            flags[slice(i << 1, n, i)] = [False] * ((n - 1) // i - 1)
    print(f'Primes up to {n:8} {count:8}')


if __name__ == '__main__':
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 4
    for i in range(0, 3):
        nsieve(10000 << (n-i))
