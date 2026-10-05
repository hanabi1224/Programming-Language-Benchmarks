// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Contributed by @bonnhatnguyen for Programming-Language-Benchmarks
// Highly optimized C++20 implementation using direct O(1) table lookup
// and block buffered I/O.

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <string_view>
#include <vector>

constexpr size_t LINE_LENGTH = 60;
constexpr uint32_t IM = 139968;
constexpr uint32_t IA = 3877;
constexpr uint32_t IC = 29573;

struct Rng {
    uint32_t state = 42;

    inline uint32_t next() noexcept {
        state = (state * IA + IC) % IM;
        return state;
    }
};

struct AminoAcid {
    char c;
    float p;
};

// Direct 136.7 KB lookup table indexed by random value in [0, IM)
// Eliminates all search loops and floating-point conversions in the generation phase.
static char iub_lookup[IM];
static char homo_lookup[IM];

static void init_lookup_table(char* table, const AminoAcid* aa, size_t count) {
    float cumulative = 0.0f;
    uint32_t current_idx = 0;
    for (size_t i = 0; i < count; ++i) {
        cumulative += aa[i].p;
        uint32_t target = static_cast<uint32_t>(std::floor(cumulative * static_cast<float>(IM)));
        if (i == count - 1 || target >= IM) {
            target = IM - 1;
        }
        while (current_idx <= target && current_idx < IM) {
            table[current_idx++] = aa[i].c;
        }
    }
}

static constexpr std::string_view ALU =
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
    "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
    "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
    "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
    "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
    "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
    "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

static constexpr AminoAcid IUB[] = {
    {'a', 0.27f},
    {'c', 0.12f},
    {'g', 0.12f},
    {'t', 0.27f},
    {'B', 0.02f},
    {'D', 0.02f},
    {'H', 0.02f},
    {'K', 0.02f},
    {'M', 0.02f},
    {'N', 0.02f},
    {'R', 0.02f},
    {'S', 0.02f},
    {'V', 0.02f},
    {'W', 0.02f},
    {'Y', 0.02f},
};

static constexpr AminoAcid HOMOSAPIENS[] = {
    {'a', 0.3029549426680f},
    {'c', 0.1979883004921f},
    {'g', 0.1975473066391f},
    {'t', 0.3015094502008f},
};

// High-capacity buffered writer
constexpr size_t BUFFER_SIZE = 256 * 1024;
static char out_buf[BUFFER_SIZE];
static size_t out_pos = 0;

static inline void flush_out() noexcept {
    if (out_pos > 0) {
        fwrite(out_buf, 1, out_pos, stdout);
        out_pos = 0;
    }
}

static inline void write_str(std::string_view s) noexcept {
    for (char c : s) {
        out_buf[out_pos++] = c;
        if (out_pos == BUFFER_SIZE) {
            flush_out();
        }
    }
}

static inline void write_char(char c) noexcept {
    out_buf[out_pos++] = c;
    if (out_pos == BUFFER_SIZE) {
        flush_out();
    }
}

static void make_fasta_repeat(std::string_view header, std::string_view seq, size_t n) {
    write_str(header);

    // Double the sequence to avoid wrapping modulo checks
    std::string double_seq;
    double_seq.reserve(seq.size() + LINE_LENGTH);
    double_seq.append(seq);
    double_seq.append(seq.substr(0, LINE_LENGTH));

    size_t seq_pos = 0;
    const size_t seq_len = seq.size();

    while (n > 0) {
        size_t chunk = std::min(LINE_LENGTH, n);
        for (size_t i = 0; i < chunk; ++i) {
            write_char(double_seq[seq_pos + i]);
        }
        write_char('\n');
        seq_pos += chunk;
        if (seq_pos >= seq_len) {
            seq_pos -= seq_len;
        }
        n -= chunk;
    }
}

static void make_fasta_random(std::string_view header, const char* lookup, Rng& rng, size_t n) {
    write_str(header);

    while (n > 0) {
        size_t chunk = std::min(LINE_LENGTH, n);
        for (size_t i = 0; i < chunk; ++i) {
            uint32_t r = rng.next();
            write_char(lookup[r]);
        }
        write_char('\n');
        n -= chunk;
    }
}

int main(int argc, char** argv) {
    size_t n = 1000;
    if (argc > 1) {
        n = static_cast<size_t>(std::atoi(argv[1]));
    }

    init_lookup_table(iub_lookup, IUB, sizeof(IUB) / sizeof(IUB[0]));
    init_lookup_table(homo_lookup, HOMOSAPIENS, sizeof(HOMOSAPIENS) / sizeof(HOMOSAPIENS[0]));

    Rng rng;

    make_fasta_repeat(">ONE Homo sapiens alu\n", ALU, n * 2);
    make_fasta_random(">TWO IUB ambiguity codes\n", iub_lookup, rng, n * 3);
    make_fasta_random(">THREE Homo sapiens frequency\n", homo_lookup, rng, n * 5);

    flush_out();
    return 0;
}
