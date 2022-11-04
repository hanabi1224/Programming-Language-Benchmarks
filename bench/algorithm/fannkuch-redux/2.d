// port from 2.zig
// TODO: fixme - need to check the posibility to pass comptime argument like in Zig
@safe:

import std;
import inteli.emmintrin, inteli.tmmintrin, inteli.smmintrin;
import core.simd;

alias Vec = ubyte16;

Vec reverseMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 0 .. n)
        v[i] = cast(ubyte) (n - i - 1);
    return v;
}

Vec rotateMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 0 .. n)
        v[i] = cast(ubyte) ((i + 1) % n);
    return v;
}

Vec nextPermMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 2 .. n + 1)
        v = applyMask(v, rotateMask(i)); //fixme
    return n;
}

Vec applyMask(Vec a, Vec mask) {
    //ubyte len = 16;
    //ubyte i = 0;
    // didn't understand logic in zig syntax..
    //while (i < len) {
    //    if (i == n)
    //        return cast(ubyte16) _mm_shuffle_epi8(cast(__m128i) a, cast(__m128i) mask);
    //    i++;
    //}
    return cast(ubyte16) _mm_shuffle_epi8(cast(__m128i) a, cast(__m128i) mask);
}

uint pfannkuchen(Vec perm) {
    uint flipsCount = 0;
    auto a = perm;
    while (true) {
        const ubyte k = cast(ubyte) a.ptr[0];
        if (k == 0)
            return flipsCount;
        a = applyMask(a, reverseMask(cast(ubyte)(k+1))); // fixme 
        flipsCount += 1;
    }
}

void main(string[] args) {
    const n = args[1].to!ubyte;
    uint maxFlipsCount = 0;
    int checksum = 0;
    Vec perm = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    ubyte[16] count = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
    bool parity = 0;
    while (true) {
        auto flipsCount = pfannkuchen(perm);
        maxFlipsCount = max(maxFlipsCount, flipsCount);
        checksum += cast(int) flipsCount * (1 - cast(int) parity * 2);
        ubyte r;
        bool end = true;
        ubyte i = 0;
        while (i < n) {
            if (count[i] != 1) {
                r = i;
                end = false;
                break;
            }
            i++;
        }
        if (end)
            break;
        perm = applyMask(perm, nextPermMask(cast(ubyte)(r + 1))); // fixme
        count[r] -= 1;
        i = 1;
        while (i < r) {
            count[i] = cast(ubyte) (i + 1);
            i++;
        }
        parity ^= 1;
    }
    writefln("%d\nPfannkuchen(%d) = %d\n", checksum, n, maxFlipsCount);
}

