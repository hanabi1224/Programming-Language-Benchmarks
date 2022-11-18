// port from 2-im.rs
//@safe:

import std;
import inteli.emmintrin, inteli.tmmintrin, inteli.smmintrin;


immutable size_t vSize = 16;
alias vItem = byte;
alias V = vItem[vSize];

enum NEXT_PERM_MASKS = nextPermMasks();
enum REVERSE_MASKS = reverseMasks();

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
        auto mask = REVERSE_MASKS[cast(size_t) k + 1];
        a = simd_shuffle(a, toMask(mask));
        flipCount += 1;
    }
}

size_t factorial(vItem n) {
    size_t res = 1;
    vItem i = 2;
    while (i <= n) {
        res *= i;
        i += 1;
    }
    return res;
}

V countAtPos (vItem n, size_t start) {
    V count;
    auto r = start;
    auto i = n;
    while (i > 0) {
        i -= 1;
        auto totalPerms = factorial(i);
        count[i] = cast(vItem)(i + 1 - (r / totalPerms));
        r %= totalPerms;
    }
    return count;
}

__m128i permWithCount(vItem n, V count) {
    V perm = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    auto i = n;
    while (i > 0) {
        bringToFront(perm[0..i - count[i - 1]], perm[i - count[i-1] .. i]);
        i -= 1;
    }
    return toMask(perm);
}

Tuple!(__m128i,bool) nextPermutation(__m128i perm, ref V count, size_t size) {
    auto r = 0;
    bool none = true;
    foreach(i; 0 .. size) {
        if (count[i] != 1) {
            r = cast(int) i;
            none = false;
            break;
        }
    }
    if (none)
        return tuple(perm, false);
    auto nextPerm = simd_shuffle(perm, toMask(NEXT_PERM_MASKS[r + 1]));
    count[r] -= 1;
    foreach(i; 0 .. r)
        count[i] = cast(vItem) (i + 1);
    return tuple(nextPerm, true);
}

__m128i toMask(V v) {
    return _mm_setr_epi8(
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13],
            v[14], v[15]
        );
}

Tuple!(int, uint) calculatePart(Tuple!(size_t, size_t, vItem) t) {
    size_t first = t[0];
    size_t last = t[1];
    vItem n = t[2];
    uint maxFlipCount = 0u;
    int checksum = 0;
    auto count = countAtPos(n, first);
    auto perm = permWithCount(n, count);
    foreach(i; first .. last) {
        auto flipCount = pfannkuchen(perm);
        if (maxFlipCount < flipCount)
            maxFlipCount = flipCount;
        if (i % 2 == 0)
            checksum += cast(int) flipCount; //mb cast(int) needed
        else
            checksum -= cast(int) flipCount; //mb cast(int) needed
        auto permNext = nextPermutation(perm, count, n);
        if (permNext[1])
            perm = permNext[0];
        else
            break;
    }
    return tuple(checksum, maxFlipCount);
}

static int adder(int a, Tuple!(int, uint) b)
{
    return a + b[0];
}
static uint maxer(uint a, Tuple!(int, uint) b)
{
    return max(a, b[1]);
}

void main(string[] args) {
    auto n = args[1].to!vItem;
    auto nCPU = totalCPUs;
    auto permsCount = factorial(n);
    auto lenPerTask = permsCount / nCPU;
    Tuple!(size_t, size_t, vItem)[] taskParams;
    foreach(first; iota(0, permsCount, lenPerTask)) {
        auto last = (first + lenPerTask).min(permsCount);
        taskParams ~= tuple(first, last, n);
    }
    auto ans = fold!(adder, maxer)(taskPool.map!(calculatePart)(taskParams), 0, 0);
    writefln("%d\nPfannkuchen(%d) = %d\n", ans[0], n, ans[1]);
}
