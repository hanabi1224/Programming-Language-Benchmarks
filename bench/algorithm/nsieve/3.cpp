// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// Wheel 8-of-30 bit-sieve C++ implementation by ndt0208

#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <vector>

static constexpr uint32_t STEPS[8] = { 3, 2, 1, 2, 1, 2, 3, 1 };

void nsieve(std::size_t max) {
    if (max <= 2) {
        std::cout << "Primes up to " << std::setw(8) << max << ' ' << std::setw(8) << 0 << '\n';
        return;
    }

    const std::size_t maxintsh = max >> 1;
    const std::size_t num_words = (maxintsh + 63) >> 6;

    static std::vector<uint64_t> bits;
    if (bits.size() < num_words) {
        bits.resize(num_words);
    }
    std::memset(bits.data(), 0, num_words * sizeof(uint64_t));
    uint64_t* const a = bits.data();

    const std::size_t q = static_cast<std::size_t>(std::sqrt(static_cast<double>(max)));
    const std::size_t qh = q >> 1;

    std::size_t step = 1;
    std::size_t factorh = 3; // 7 >> 1

    while (factorh <= qh) {
        const std::size_t wIdx = factorh >> 6;
        const std::size_t bIdx = factorh & 63;

        if ((a[wIdx] & (1ULL << bIdx)) == 0) {
            const std::size_t factor = (factorh << 1) + 1;
            std::size_t istep = step;
            std::size_t i = (factor * factor) >> 1;

            const std::size_t s0 = factor * STEPS[istep];
            const std::size_t s1 = factor * STEPS[(istep + 1) & 7];
            const std::size_t s2 = factor * STEPS[(istep + 2) & 7];
            const std::size_t s3 = factor * STEPS[(istep + 3) & 7];
            const std::size_t s4 = factor * STEPS[(istep + 4) & 7];
            const std::size_t s5 = factor * STEPS[(istep + 5) & 7];
            const std::size_t s6 = factor * STEPS[(istep + 6) & 7];
            const std::size_t s7 = factor * STEPS[(istep + 7) & 7];
            const std::size_t cycle = factor * 15;

            while (i + cycle < maxintsh) {
                a[i >> 6] |= (1ULL << (i & 63)); i += s0;
                a[i >> 6] |= (1ULL << (i & 63)); i += s1;
                a[i >> 6] |= (1ULL << (i & 63)); i += s2;
                a[i >> 6] |= (1ULL << (i & 63)); i += s3;
                a[i >> 6] |= (1ULL << (i & 63)); i += s4;
                a[i >> 6] |= (1ULL << (i & 63)); i += s5;
                a[i >> 6] |= (1ULL << (i & 63)); i += s6;
                a[i >> 6] |= (1ULL << (i & 63)); i += s7;
            }
            while (i < maxintsh) {
                a[i >> 6] |= (1ULL << (i & 63));
                i += factor * STEPS[istep];
                istep = (istep + 1) & 7;
            }
        }

        factorh += STEPS[step];
        step = (step + 1) & 7;
    }

    std::size_t count = (max > 2) + (max > 3) + (max > 5);
    std::size_t factor = 7;

    while (factor + 30 < max) {
        std::size_t h0 = factor >> 1; factor += 4;
        std::size_t h1 = factor >> 1; factor += 2;
        std::size_t h2 = factor >> 1; factor += 4;
        std::size_t h3 = factor >> 1; factor += 2;
        std::size_t h4 = factor >> 1; factor += 4;
        std::size_t h5 = factor >> 1; factor += 6;
        std::size_t h6 = factor >> 1; factor += 2;
        std::size_t h7 = factor >> 1; factor += 6;

        count += ((a[h0 >> 6] >> (h0 & 63)) ^ 1ULL) & 1ULL;
        count += ((a[h1 >> 6] >> (h1 & 63)) ^ 1ULL) & 1ULL;
        count += ((a[h2 >> 6] >> (h2 & 63)) ^ 1ULL) & 1ULL;
        count += ((a[h3 >> 6] >> (h3 & 63)) ^ 1ULL) & 1ULL;
        count += ((a[h4 >> 6] >> (h4 & 63)) ^ 1ULL) & 1ULL;
        count += ((a[h5 >> 6] >> (h5 & 63)) ^ 1ULL) & 1ULL;
        count += ((a[h6 >> 6] >> (h6 & 63)) ^ 1ULL) & 1ULL;
        count += ((a[h7 >> 6] >> (h7 & 63)) ^ 1ULL) & 1ULL;
    }

    std::size_t cstep = 1;
    while (factor < max) {
        std::size_t half = factor >> 1;
        count += ((a[half >> 6] >> (half & 63)) ^ 1ULL) & 1ULL;
        factor += STEPS[cstep] << 1;
        cstep = (cstep + 1) & 7;
    }

    std::cout << "Primes up to " << std::setw(8) << max << ' ' << std::setw(8) << count << '\n';
}

int main(int argc, char **argv) {
    int count = 4;
    if (argc > 1) {
        count = std::atoi(argv[1]);
    }
    for (std::size_t i = 0; i < 3; ++i) {
        nsieve(10000 << (count - i));
    }
    return 0;
}
