const A = 1103515245;
const C = 12345;
const M = 2 ** 31;

const K = 1 << 16;
const LF = ~~(A / K);
const RT = A % K;

function *createLCG(seed) {
    while (true) {
        const lf = (seed * LF) % M;
        const rt = seed * RT;
        seed = (lf * K + rt + C) % M;
        yield seed;
    }
}

class LinkedListNode {
    data;
    prev = undefined;
    next = undefined;
    constructor(data) {
        this.data = data;
    }
}
class LinkedList {
    len = 0;
    head = undefined;
    tail = undefined;
    add(data) {
        const node = new LinkedListNode(data);
        this.__add_node(node);
        this.len += 1;
        return node;
    }
    __add_node(node) {
        if (this.head == undefined) {
            this.head = node;
            node.prev = undefined;
        }
        else if (this.tail != undefined) {
            node.prev = this.tail;
            this.tail.next = node;
        }
        this.tail = node;
        node.next = undefined;
    }
    __remove(node) {
        if (this.head == node) {
            this.head = node.next;
        }
        if (this.tail == node) {
            this.tail = node.prev;
        }
        if (node.prev != undefined) {
            node.prev.next = node.next;
        }
        if (node.next != undefined) {
            node.next.prev = node.prev;
        }
    }
    move_to_end(node) {
        this.__remove(node);
        this.__add_node(node);
    }
}
class Pair {
    key;
    value;
    constructor(key, value) {
        this.key = key;
        this.value = value;
    }
}
class LRU {
    size;
    keys;
    entries;
    constructor(size) {
        this.size = size;
        this.keys = new Map();
        this.entries = new LinkedList();
    }
    get(key) {
        const node = this.keys.get(key);
        if (node == undefined) {
            return undefined;
        }
        this.entries.move_to_end(node);
        return node.data.value;
    }
    put(key, value) {
        const node = this.keys.get(key);
        if (node != undefined) {
            node.data.value = value;
            this.entries.move_to_end(node);
        }
        else if (this.size == this.entries.len) {
            const head = this.entries.head;
            this.keys.delete(head.data.key);
            head.data.key = key;
            head.data.value = value;
            this.entries.move_to_end(head);
            this.keys.set(key, head);
        }
        else {
            this.keys.set(key, this.entries.add(new Pair(key, value)));
        }
    }
}
function main() {
    const size = +process.argv[2] || 100
    const n = +process.argv[3] || 100
    const mod = size * 10;
    const rng0 = createLCG(0);
    const rng1 = createLCG(1);
    const lru = new LRU(size);
    let hit = 0;
    let missed = 0;
    for (let i = 0; i < n; i++) {
        const n0 = rng0.next().value % mod;
        lru.put(n0, n0);
        const n1 = rng1.next().value % mod;
        if (lru.get(n1) == undefined) {
            missed += 1;
        }
        else {
            hit += 1;
        }
    }
    console.log(`${hit}\n${missed}`);
}
main();
