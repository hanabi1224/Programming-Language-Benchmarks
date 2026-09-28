// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// Contributed by ndt0208
// Multi-threaded binary trees benchmark with thread-local reusable bump allocator arena

#include <iostream>
#include <cstdlib>
#include <vector>
#include <omp.h>

struct Node {
    Node *left;
    Node *right;

    int check() const {
        if (left) {
            return left->check() + right->check() + 1;
        }
        return 1;
    }
};

class MemoryPool {
private:
    char* buffer = nullptr;
    size_t capacity = 0;
    size_t offset = 0;

public:
    MemoryPool() = default;
    ~MemoryPool() {
        if (buffer) std::free(buffer);
    }

    void ensure_capacity(size_t bytes) {
        if (bytes > capacity) {
            if (buffer) std::free(buffer);
            capacity = bytes;
            buffer = static_cast<char*>(std::malloc(capacity));
        }
        offset = 0;
    }

    inline void* allocate(size_t bytes) {
        bytes = (bytes + 7) & ~size_t(7);
        void* ptr = buffer + offset;
        offset += bytes;
        return ptr;
    }

    inline void reset() {
        offset = 0;
    }
};

Node* create_tree(int depth, MemoryPool& pool) {
    Node* node = static_cast<Node*>(pool.allocate(sizeof(Node)));
    if (depth > 0) {
        node->left = create_tree(depth - 1, pool);
        node->right = create_tree(depth - 1, pool);
    } else {
        node->left = nullptr;
        node->right = nullptr;
    }
    return node;
}

static constexpr int MIN_DEPTH = 4;

int main(int argc, char** argv) {
    int n = 10;
    if (argc > 1) {
        n = std::atoi(argv[1]);
    }
    int max_depth = (MIN_DEPTH + 2 > n) ? MIN_DEPTH + 2 : n;

    {
        int stretch_depth = max_depth + 1;
        size_t required = (size_t(1) << (stretch_depth + 1)) * sizeof(Node);
        MemoryPool pool;
        pool.ensure_capacity(required);
        Node* stretch_tree = create_tree(stretch_depth, pool);
        std::cout << "stretch tree of depth " << stretch_depth << "\t check: " << stretch_tree->check() << '\n';
    }

    size_t long_lived_bytes = (size_t(1) << (max_depth + 1)) * sizeof(Node);
    MemoryPool long_lived_pool;
    long_lived_pool.ensure_capacity(long_lived_bytes);
    Node* long_lived_tree = create_tree(max_depth, long_lived_pool);

    int num_depths = (max_depth - MIN_DEPTH) / 2 + 1;
    std::vector<int> results(num_depths);

    for (int d_idx = 0; d_idx < num_depths; ++d_idx) {
        int d = MIN_DEPTH + d_idx * 2;
        int iterations = 1 << (max_depth - d + MIN_DEPTH);
        size_t required_bytes = (size_t(1) << (d + 1)) * sizeof(Node);
        int chk = 0;

        #pragma omp parallel reduction(+:chk)
        {
            MemoryPool thread_pool;
            thread_pool.ensure_capacity(required_bytes);

            #pragma omp for schedule(guided)
            for (int i = 0; i < iterations; ++i) {
                thread_pool.reset();
                Node* a = create_tree(d, thread_pool);
                chk += a->check();
            }
        }
        results[d_idx] = chk;
    }

    for (int d_idx = 0; d_idx < num_depths; ++d_idx) {
        int d = MIN_DEPTH + d_idx * 2;
        int iterations = 1 << (max_depth - d + MIN_DEPTH);
        std::cout << iterations << "\t trees of depth " << d << "\t check: " << results[d_idx] << '\n';
    }

    std::cout << "long lived tree of depth " << max_depth << "\t check: " << long_lived_tree->check() << '\n';
    return 0;
}
