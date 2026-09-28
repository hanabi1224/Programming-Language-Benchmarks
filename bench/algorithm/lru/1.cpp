// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// LRU Cache implementation in C++ by ndt0208

#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <vector>

struct LCG {
    uint32_t seed;
    explicit LCG(uint32_t s) : seed(s) {}

    inline uint32_t next() {
        constexpr uint32_t A = 1103515245u;
        constexpr uint32_t C = 12345u;
        constexpr uint32_t M = 1u << 31;
        seed = (A * seed + C) % M;
        return seed;
    }
};

struct Node {
    uint32_t key;
    uint32_t val;
    uint32_t prev;
    uint32_t next;
    uint32_t hnext;
};

class LRUCache {
private:
    uint32_t capacity;
    uint32_t count;
    uint32_t mask;
    std::vector<uint32_t> buckets;
    std::vector<Node> nodes;

    inline uint32_t hash(uint32_t k) const {
        return (k * 2654435761u) & mask;
    }

    inline void unlink_node(uint32_t idx) {
        uint32_t p = nodes[idx].prev;
        uint32_t n = nodes[idx].next;
        nodes[p].next = n;
        nodes[n].prev = p;
    }

    inline void insert_head(uint32_t idx) {
        uint32_t head = nodes[0].next;
        nodes[idx].prev = 0;
        nodes[idx].next = head;
        nodes[0].next = idx;
        nodes[head].prev = idx;
    }

public:
    explicit LRUCache(uint32_t cap) : capacity(cap), count(0) {
        uint32_t bsize = 16;
        while (bsize < cap * 2) {
            bsize <<= 1;
        }
        mask = bsize - 1;
        buckets.assign(bsize, 0);
        nodes.resize(cap + 1);
        nodes[0].prev = 0;
        nodes[0].next = 0;
    }

    inline bool get(uint32_t key) {
        uint32_t b = hash(key);
        uint32_t idx = buckets[b];
        while (idx != 0 && nodes[idx].key != key) {
            idx = nodes[idx].hnext;
        }
        if (idx == 0) return false;

        unlink_node(idx);
        insert_head(idx);
        return true;
    }

    inline void put(uint32_t key, uint32_t val) {
        uint32_t b = hash(key);
        uint32_t idx = buckets[b];
        while (idx != 0 && nodes[idx].key != key) {
            idx = nodes[idx].hnext;
        }

        if (idx != 0) {
            nodes[idx].val = val;
            unlink_node(idx);
            insert_head(idx);
            return;
        }

        if (count < capacity) {
            count++;
            idx = count;
        } else {
            idx = nodes[0].prev; // LRU tail
            uint32_t old_b = hash(nodes[idx].key);
            if (buckets[old_b] == idx) {
                buckets[old_b] = nodes[idx].hnext;
            } else {
                uint32_t curr = buckets[old_b];
                while (nodes[curr].hnext != idx) {
                    curr = nodes[curr].hnext;
                }
                nodes[curr].hnext = nodes[idx].hnext;
            }
            unlink_node(idx);
        }

        nodes[idx].key = key;
        nodes[idx].val = val;
        nodes[idx].hnext = buckets[b];
        buckets[b] = idx;
        insert_head(idx);
    }
};

int main(int argc, char **argv) {
    uint32_t size = 100;
    uint32_t n = 1000;
    if (argc > 1) {
        size = std::strtoul(argv[1], nullptr, 10);
    }
    if (argc > 2) {
        n = std::strtoul(argv[2], nullptr, 10);
    }

    const uint32_t modular = size * 10;
    LCG rng0(0);
    LCG rng1(1);

    LRUCache lru(size);
    uint32_t hit = 0;
    uint32_t missed = 0;

    for (uint32_t i = 0; i < n; ++i) {
        uint32_t n0 = rng0.next() % modular;
        lru.put(n0, n0);
        uint32_t n1 = rng1.next() % modular;
        if (lru.get(n1)) {
            hit++;
        } else {
            missed++;
        }
    }

    std::cout << hit << '\n' << missed << '\n';
    return 0;
}
