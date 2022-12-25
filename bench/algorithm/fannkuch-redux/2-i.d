// port from 2-i.rs

import std;
import inteli.emmintrin, inteli.tmmintrin, inteli.smmintrin;


immutable size_t vSize = 16;
alias vItem = ubyte;
alias V = vItem[vSize];

static immutable NEXT_PERM_MASKS = nextPermMasks();
static immutable REVERSE_MASKS = reverseMasks();

V shuffle(V a, V mask) {
    V r;
    foreach (i; 0 .. vSize)
        r[i] = a[mask[i]]; // mb need cast
    return r;
}

__m128i simd_shuffle(__m128i a, __m128i mask) {
    return _mm_shuffle_epi8(a, mask);
}

V reverseMask(vItem n) {
    V v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(i; 0 .. n)
        v[i] = cast(vItem)(n - i - 1);
    return v;
}

V rotateMask(vItem n) {
    V v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(i; 0 .. n)
        v[i] = cast(vItem)((i + 1) % n);
    return v;
}

V[vSize] reverseMasks() {
    V[vSize] v;
    foreach(i; 0 .. vSize)
        v[i] = reverseMask(cast(vItem) i);
    return v;
}

V nextPermMask(vItem n) {
    V v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(i; 2 .. n + 1)
        v = shuffle(v, rotateMask(cast(vItem)i));
    return v;
}

V[vSize] nextPermMasks() {
    V[vSize] v;
    size_t i = 0;
    while (i < vSize) {
        v[i] = nextPermMask(cast(vItem) i);
        i++;
    }
    return v;
}

uint pfannkuchen(__m128i perm) { // try fat pointer
    uint flipCount = 0u;
    auto a = perm;
    while (true) {
        const k = _mm_extract_epi8(a, 0);
        if (k == 0)
            return flipCount;
        a = simd_shuffle(a, toMask(REVERSE_MASKS[cast(size_t) k + 1]));
        flipCount += 1;
    }
}

__m128i toMask(V v) {
    return _mm_setr_epi8(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13],
            v[14], v[15]
        );
}

Tuple!(int, uint) calculate(size_t n) {
    uint maxFlipCount = 0u;
    int checksum = 0;
    auto perm = _mm_setr_epi8(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
    V count = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
    bool parity = 0;
    while (true) {
        auto flipCount = pfannkuchen(perm);
        if (maxFlipCount < flipCount)
            maxFlipCount = flipCount;
        checksum += cast(int) flipCount * (1 - cast(int) parity * 2);
        ubyte r;
        bool end = true;
        foreach(i, v; count[0 .. n]) {
            if (v != 1) {
                r = cast(ubyte) i;
                end = false;
                break;
            }
        }
        if (end)
            break;
        perm = simd_shuffle(perm, toMask(NEXT_PERM_MASKS[r + 1]));
        
        count[r] -= 1;
        ubyte i = 1;
        while (i < r) {
            count[i] = cast(ubyte) (i + 1); // in 2.zig i + 2
            i++;
        }
        parity ^= 1;
    }
    return tuple(checksum, maxFlipCount);
}

void main(string[] args) {
    const n = args[1].to!ubyte;
    Tuple!(int, "checksum", uint, "maxFlipCount") ans = calculate(n);
    writefln("%d\nPfannkuchen(%d) = %d", ans.checksum, n, ans.maxFlipCount);
}
