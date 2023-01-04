import sys
from datetime import datetime

def main():
    n = 100 if len(sys.argv) < 2 else int(sys.argv[1])
    ch = generate()
    for i in range(0, n):
        prime = ch.__next__()
        print(prime)
        ch = filter(ch, prime)


def generate():
    i = 2
    while True:
        yield i
        i += 1


def filter(ch, prime):
    for i in ch:
        if i % prime != 0:
            yield i


if __name__ == '__main__':
    with open("ready", "w") as f:
        f.write(str(round(datetime.utcnow().timestamp() * 1000)))

    sys.setrecursionlimit(5000)
    main()
