const A: bigint = 1103515245n
const C: bigint = 12345n
const M: bigint = 1n << 31n

class LCG {
    private seed: bigint
    constructor(seed: bigint) {
        this.seed = seed
    }

    next(): bigint {
        this._lcg()
        return this.seed
    }

    _lcg() {
        this.seed = (A * this.seed + C) % M
    }
}

class LRU {
    private size
    private m
    constructor(size: number) {
        this.size = size;
        this.m = new Map()
    }

    get(key: bigint) {
        if (this.m.has(key)) {
            const v = this.m.get(key)
            this.m.delete(key)
            this.m.set(key, v)
            return v
        }
        return null
    }

    put(key: bigint, value: bigint) {
        if (this.m.has(key)) {
            this.m.delete(key)
        } else if (this.m.size >= this.size) {
            this.m.delete(this.m.keys().next().value)
        }
        this.m.set(key, value)
    }
}

function main() {
    const n = +Deno.args[0] || 100

    const rng0 = new LCG(0n)
    const rng1 = new LCG(1n)
    const lru = new LRU(10)
    let hit = 0
    let missed = 0

    for (var i = 0; i < n; i++) {
        const n0 = rng0.next() % 100n
        lru.put(n0, n0)
        const n1 = rng1.next() % 100n
        if (lru.get(n1) === null) {
            missed += 1
        } else {
            hit += 1
        }
    }

    console.log(`${hit}\n${missed}`)
}

main()
