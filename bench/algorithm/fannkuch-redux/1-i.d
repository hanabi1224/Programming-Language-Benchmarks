// Based on 1-im.cpp implementation
//@safe:

import std.stdio : writeln;
import std.stdint : int_fast8_t, int_fast64_t;
import std.algorithm.mutation : bringToFront;
import std.conv : to;
import inteli.emmintrin, inteli.tmmintrin, inteli.smmintrin;

alias smallInt = int_fast8_t;
alias bigInt = int_fast64_t;

immutable static smallInt maxN = 16;
immutable static int maxBlocks = 24;

class Masks
{
    __m128i[16] masksReverse;
    __m128i[16] masksShift;

    this()
    {
        masksReverse = [
        __m128i.init,
        _mm_setr_epi8(1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(2, 1, 0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(3, 2, 1, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(4, 3, 2, 1, 0, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(5, 4, 3, 2, 1, 0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(6, 5, 4, 3, 2, 1, 0, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(7, 6, 5, 4, 3, 2, 1, 0, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(8, 7, 6, 5, 4, 3, 2, 1, 0, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 11, 12, 13, 14, 15),
        _mm_setr_epi8(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 12, 13, 14, 15),
        _mm_setr_epi8(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 13, 14, 15),
        _mm_setr_epi8(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 14, 15),
        _mm_setr_epi8(14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 15),
        _mm_setr_epi8(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0),
    ];
    masksShift = [
        __m128i.init,
        _mm_setr_epi8(1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 0, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 0, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 0, 7, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 0, 8, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 0, 9, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 10, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 11, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 12, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 13, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 0, 14, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 15),
        _mm_setr_epi8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0),
    ];
    }
}

    

// check better implementation. maybe CT, if it used in zig versions
bigInt[maxN] computeFactorials(smallInt n)
{
    bigInt[maxN] factorials;
    factorials[0] = 1;
    for (smallInt i = 1; i <= n; ++i)
        factorials[i] = factorials[i - 1] * i;
    return factorials;
}

int[2] getBlocksAndSize(smallInt n, ref bigInt[maxN] factorials)
{
    int blocks = maxBlocks;
    if (blocks > factorials[n])
    blocks = 1;
    int blockSize = cast(int) factorials[n] / blocks;
    return [blocks, blockSize];
}

bigInt[maxN] createCount(smallInt n, bigInt start, ref bigInt[maxN] factorials)
{
    bigInt[maxN] count;
    for (smallInt i = cast(smallInt)(n - 1); i >= 0; i--)
    {
        bigInt d = start / factorials[i];
        start = start % factorials[i];
        count[i] = d;
    }
    return count;
}

__m128i createCurrent(smallInt n, ref bigInt[maxN] count)
{
    align(16) smallInt[maxN] currentAux = [
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
    ];
    for (smallInt i = cast(smallInt)(n - 1); i >= 0; --i)
        bringToFront(currentAux[0 .. count[i]], currentAux[count[i] .. i + 1]);
    __m128i current = _mm_load_si128(cast(__m128i*)(currentAux.ptr));
    return current;
}

pragma(inline, true):
__m128i incrementPermutation(ref bigInt[maxN] count, __m128i current, ref Masks masks)
{
    for (smallInt i = 1;; ++i)
    {
        current = _mm_shuffle_epi8(current, masks.masksShift[i]);
        if (++count[i] <= i)
        break;
        count[i] = 0;
    }
    return current;
}

pragma(inline, true):
__m128i reverse(__m128i x, smallInt idx, ref Masks masks)
{
    return _mm_shuffle_epi8(x, masks.masksReverse[idx]);
}

void main(string[] args)
{
    immutable smallInt n = args[1].to!smallInt;
    auto factorials = computeFactorials(n);
    auto blockSize = getBlocksAndSize(n, factorials)[1];

    smallInt maxFlips = 0;
    bigInt checksum = 0;

    for (bigInt blockStart = 0; blockStart < factorials[n]; blockStart += blockSize)
    {
        Masks masks = new Masks();
        bigInt[maxN] count = createCount(n, blockStart, factorials);

        __m128i current = createCurrent(n, count);
        __m128i currentStart = current;

        smallInt first = cast(smallInt) _mm_extract_epi8(current, 0);

        bigInt crtIdx = blockStart;
        bigInt blockEnd = blockStart + blockSize;

        while (crtIdx < blockEnd)
        {
            if (first > 0)
            {
                smallInt flips = 0;
                while (first != 0)
                {
                    auto next = (*cast(char[16]*)(current.ptr))[first]; // or try Union here
                    current = reverse(current, first, masks);
                    first = cast(smallInt*) next;
                    ++flips;
                }

                checksum += (crtIdx % 2) == 0 ? flips : -flips;

                if (flips > maxFlips)
                    maxFlips = flips;
            }

            current = incrementPermutation(count, currentStart, masks);
            currentStart = current;

            first = cast(smallInt) _mm_extract_epi8(current, 0);
            ++crtIdx;
        }
    }
    writeln(cast(int) checksum, "\nPfannkuchen(", n, ") = ", cast(int) maxFlips);
}
