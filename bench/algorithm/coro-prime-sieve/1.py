import sys
import asyncio
from datetime import datetime


async def main():
    n = 100 if len(sys.argv) < 2 else int(sys.argv[1])
    ch = generate()
    for i in range(0, n):
        prime = await ch.__anext__()
        print(prime)
        ch = filter(ch, prime)


async def generate():
    i = 2
    while True:
        yield i
        i += 1


async def filter(ch, prime):
    async for i in ch:
        if i % prime != 0:
            yield i

if __name__ == '__main__':
    with open("ready", "w") as f:
        f.write(str(round(datetime.utcnow().timestamp() * 1000)))

    sys.setrecursionlimit(5000)
    asyncio.run(main())
