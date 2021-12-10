const A = 1103515245n
const C = 12345n
const M = 1n << 31n

class LCG {
    constructor(seed) {
        this.seed = seed
    }

    next() {
        this._lcg()
        return this.seed
    }

    _lcg() {
        this.seed = (A * this.seed + C) % M
    }
}

class LRU {
    constructor(size) {
        this.size = size;
        this.m = new Map()
    }

    get(key) {
        if (this.m.has(key)) {
            const v = this.m.get(key)
            this.m.delete(key)
            this.m.set(key, v)
            return v
        }
        return null
    }

    put(key, value) {
        if (this.m.has(key)) {
            this.m.delete(key)
        } else if (this.m.size >= this.size) {
            this.m.delete(this.m.keys().next().value)
        }
        this.m.set(key, value)
    }
}

function main() {
    const n = +process.argv[2] || 100

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

    console.log(hit)
    console.log(missed)
}

main()
