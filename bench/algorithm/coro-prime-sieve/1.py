import sys
import asyncio
import itertools


async def main():
    n = 100 if len(sys.argv) < 2 else int(sys.argv[1])
    out_q = asyncio.Queue(1)
    asyncio.create_task(generate(out_q))
    for _ in range(n):
        prime = await out_q.get()
        print(prime)
        in_q, out_q = out_q, asyncio.Queue(1)
        asyncio.create_task(filter(in_q, out_q, prime))


async def generate(q):
    for i in itertools.count(2):
        await q.put(i)

async def filter(in_q, out_q, prime):
    while True:
        i = await in_q.get()
        if i % prime:
            await out_q.put(i)

if __name__ == '__main__':
    asyncio.run(main())