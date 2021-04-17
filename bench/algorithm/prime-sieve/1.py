from sys import argv
import asyncio


async def main():
    n = 100 if len(argv) < 2 else int(argv[1])
    ch = generate()
    for i in range(0, n):
        prime = await ch.__anext__()
        print(prime)
        ch1 = filter(ch, prime)
        ch = ch1


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
    asyncio.run(main())