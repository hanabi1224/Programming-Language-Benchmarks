const A: bigint = 1103515245n
const C: bigint = 12345n
const M: bigint = 1n << 31n

class LinkedListNode<T> {
    data: T
    prev?: LinkedListNode<T> = undefined
    next?: LinkedListNode<T> = undefined
    constructor(data: T) {
        this.data = data
    }
}

class LinkedList<T> {
    len = 0
    head?: LinkedListNode<T> = undefined
    tail?: LinkedListNode<T> = undefined

    add(data: T): LinkedListNode<T> {
        const node = new LinkedListNode<T>(data)
        this.__add_node(node)
        this.len += 1
        return node
    }

    __add_node(node: LinkedListNode<T>) {
        if (this.head == undefined) {
            this.head = node
            node.prev = undefined
        } else if (this.tail != undefined) {
            node.prev = this.tail
            this.tail.next = node
        }
        this.tail = node
        node.next = undefined
    }

    __remove(node: LinkedListNode<T>) {
        if (this.head == node) {
            this.head = node.next
        }
        if (this.tail == node) {
            this.tail = node.prev
        }
        if (node.prev != undefined) {
            node.prev.next = node.next
        }
        if (node.next != undefined) {
            node.next.prev = node.prev
        }
    }

    move_to_end(node: LinkedListNode<T>) {
        this.__remove(node)
        this.__add_node(node)
    }
}

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

class Pair<K, V>{
    key: K
    value: V
    constructor(key: K, value: V) {
        this.key = key
        this.value = value
    }
}

class LRU<K, V> {
    private size: number
    private keys: Map<K, LinkedListNode<Pair<K, V>>>
    private entries: LinkedList<Pair<K, V>>
    constructor(size: number) {
        this.size = size;
        this.keys = new Map()
        this.entries = new LinkedList()
    }

    get(key: K): V | undefined {
        const node = this.keys.get(key)
        if (node == undefined) {
            return undefined;
        }
        this.entries.move_to_end(node)
        return node.data.value
    }

    put(key: K, value: V) {
        const node = this.keys.get(key)
        if (node != undefined) {
            node.data.value = value
            this.entries.move_to_end(node)
        } else if (this.size == this.entries.len) {
            const head = this.entries.head!
            this.keys.delete(head.data.key)
            head.data.key = key
            head.data.value = value
            this.entries.move_to_end(head)
            this.keys.set(key, head)
        } else {
            this.keys.set(key, this.entries.add(new Pair(key, value)))
        }
    }
}

function main() {
    const size = +Deno.args[0] || 100
    const n = +Deno.args[1] || 100
    const mod = BigInt(size * 10)

    const rng0 = new LCG(0n)
    const rng1 = new LCG(1n)
    const lru = new LRU(size)
    let hit = 0
    let missed = 0

    for (var i = 0; i < n; i++) {
        const n0 = rng0.next() % mod
        lru.put(n0, n0)
        const n1 = rng1.next() % mod
        if (lru.get(n1) == undefined) {
            missed += 1
        } else {
            hit += 1
        }
    }

    console.log(`${hit}\n${missed}`)
}

main()
