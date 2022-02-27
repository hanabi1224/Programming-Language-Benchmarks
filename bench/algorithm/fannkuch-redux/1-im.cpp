// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Contributed by Andrei Simion (with patch from Vincent Yu)
// Based on "fannkuch-redux C++ g++ #5", contributed by Dave Compton,
// which in turn was based on the C program by Jeremy Zerfasm
// which in turn was based on the Ada program by Jonathan Parker and
// Georg Bauhaus which in turn was based on code by Dave Fladebo,
// Eckehard Berns, Heiner Marxen, Hongwei Xi, and The Anh Tran and
// also the Java program by Oleg Mazurov.

#include <algorithm>
#include <array>
#include <immintrin.h>
#include <iostream>

using small_int_t = char;
using big_int_t = int64_t;

constexpr small_int_t max_n = 16;
constexpr int max_blocks = 24;

struct Masks {
  __m128i masks_reverse[16];
  __m128i masks_shift[16];

  Masks() {
    // first elements are never accessed
    masks_reverse[1] =
        _mm_setr_epi8(1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[2] =
        _mm_setr_epi8(2, 1, 0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[3] =
        _mm_setr_epi8(3, 2, 1, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[4] =
        _mm_setr_epi8(4, 3, 2, 1, 0, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[5] =
        _mm_setr_epi8(5, 4, 3, 2, 1, 0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[6] =
        _mm_setr_epi8(6, 5, 4, 3, 2, 1, 0, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[7] =
        _mm_setr_epi8(7, 6, 5, 4, 3, 2, 1, 0, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[8] =
        _mm_setr_epi8(8, 7, 6, 5, 4, 3, 2, 1, 0, 9, 10, 11, 12, 13, 14, 15);
    masks_reverse[9] =
        _mm_setr_epi8(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 10, 11, 12, 13, 14, 15);
    masks_reverse[10] =
        _mm_setr_epi8(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 11, 12, 13, 14, 15);
    masks_reverse[11] =
        _mm_setr_epi8(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 12, 13, 14, 15);
    masks_reverse[12] =
        _mm_setr_epi8(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 13, 14, 15);
    masks_reverse[13] =
        _mm_setr_epi8(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 14, 15);
    masks_reverse[14] =
        _mm_setr_epi8(14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 15);
    masks_reverse[15] =
        _mm_setr_epi8(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0);

    masks_shift[1] =
        _mm_setr_epi8(1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[2] =
        _mm_setr_epi8(1, 2, 0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[3] =
        _mm_setr_epi8(1, 2, 3, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[4] =
        _mm_setr_epi8(1, 2, 3, 4, 0, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[5] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[6] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 0, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[7] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 0, 8, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[8] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 0, 9, 10, 11, 12, 13, 14, 15);
    masks_shift[9] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 10, 11, 12, 13, 14, 15);
    masks_shift[10] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 11, 12, 13, 14, 15);
    masks_shift[11] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 12, 13, 14, 15);
    masks_shift[12] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 13, 14, 15);
    masks_shift[13] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 0, 14, 15);
    masks_shift[14] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 15);
    masks_shift[15] =
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0);
  }
};

std::array<big_int_t, max_n> compute_factorials(small_int_t n) {
  std::array<big_int_t, max_n> factorials;

  factorials[0] = 1;
  for (small_int_t i = 1; i <= n; ++i)
    factorials[i] = factorials[i - 1] * i;

  return factorials;
}

std::array<int, 2>
get_blocks_and_size(small_int_t n, std::array<big_int_t, max_n> &factorials) {
  int blocks = max_blocks;
  if (blocks > factorials[n])
    blocks = 1;
  int block_size = factorials[n] / blocks;

  return {blocks, block_size};
}

std::array<big_int_t, max_n>
create_count(small_int_t n, big_int_t start,
             std::array<big_int_t, max_n> &factorials) {
  std::array<big_int_t, max_n> count;

  for (small_int_t i = n - 1; i >= 0; --i) {
    big_int_t d = start / factorials[i];
    start = start % factorials[i];
    count[i] = d;
  }

  return count;
}

__m128i create_current(small_int_t n, std::array<big_int_t, max_n> &count) {
  // not a "hotspot", using intrinsics for rotations don't bring
  // much benefit here considering the increased complexity
  alignas(16) std::array<small_int_t, max_n> current_aux = {
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

  for (small_int_t i = n - 1; i >= 0; --i) {
    std::rotate(current_aux.begin(), current_aux.begin() + count[i],
                current_aux.begin() + i + 1);
  }
  __m128i current = _mm_load_si128((__m128i *)current_aux.data());
  return current;
}

inline __m128i increment_permutation(std::array<big_int_t, max_n> &count,
                                     __m128i current, const Masks &masks) {
  for (small_int_t i = 1;; ++i) {
    current = _mm_shuffle_epi8(current, masks.masks_shift[i]);
    if (++count[i] <= i)
      break;
    count[i] = 0;
  }
  return current;
}

inline __m128i reverse(__m128i x, small_int_t idx, const Masks &masks) {
  return _mm_shuffle_epi8(x, masks.masks_reverse[idx]);
}

int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  auto factorials = compute_factorials(n);
  auto block_size = get_blocks_and_size(n, factorials)[1];

  small_int_t max_flips = 0;
  big_int_t checksum = 0;

// iterate over each block.
#pragma omp parallel for reduction(max : max_flips) reduction(+ : checksum)
  for (big_int_t block_start = 0; block_start < factorials[n];
       block_start += block_size) {
    Masks masks;

    std::array<big_int_t, max_n> count =
        create_count(n, block_start, factorials);

    __m128i current = create_current(n, count);
    __m128i current_start = current;

    small_int_t first = _mm_extract_epi8(current, 0);

    // iterate over each permutation in the block.
    big_int_t crt_idx = block_start;
    big_int_t block_end = block_start + block_size;

    while (crt_idx < block_end) {
      if (first > 0) {
        small_int_t flips = 0;
        while (first != 0) {
          // patch from Vincent Yu
          auto next = reinterpret_cast<char(&)[16]>(current)[first];
          current = reverse(current, first, masks);
          first = next;
          ++flips;
        }

        checksum += (crt_idx % 2) == 0 ? flips : -flips;

        if (flips > max_flips)
          max_flips = flips;
      }

      current = increment_permutation(count, current_start, masks);
      current_start = current;

      first = _mm_extract_epi8(current, 0);
      ++crt_idx;
    }
  }
  std::cout << checksum << "\nPfannkuchen(" << n << ") = " << (int)max_flips
            << "\n";
}
