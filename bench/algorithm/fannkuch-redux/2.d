// port from 2.zig
@safe:

import std;

version(LDC) {
    import ldc.simd;
    import core.simd;
}
version(D_SIMD) {
    import core.simd;
}


alias Vec = ubyte16;

Vec reverseMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 0 .. n)
        v.ptr[i] = n - i - 1;
    return v;
}

Vec rotateMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 0 .. n)
        v.ptr[i] = (i + 1) % n;
    return v;
}

Vec nextPermMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 2 .. n + 1)
        v = applyMask(v, i, rotateMask(i)); //fixme
    return n;
}

Vec applyMask(Vec a, ubyte n, Vec mask) {
    ubyte len = 16;
    ubyte i = 0;
    while (i < len) {
        if (i == n)
            return shufflevector!(ubyte16, mask)(a, a);
        i++;
    }
}

uint pfannkuchen(Vec perm) {
    uint flipCount = 0;
    auto a = perm;
    while (true) {
        const k = a.array[0];
        if (k == 0)
            return flipCount;
        a = applyMask(a, k + 1, reverseMask(k+1)); // fixme 
        flipCount += 1;
    }
}

void main(string[] args) {
    const n = args[1].to!ubyte;
    uint maxFlipCount = 0;
    int checksum = 0;
    Vec perm = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    ubyte[16] count = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
    bool parity = 0;
    while (true) {
        flipCount = pfannkuchen(perm);
        maxFlipCount = max(maxFlipCount, flipCount);
        checksum += cast(int) flipCount * (1 - cast(int) parity * 2);
        const ubyte r;
        foreach(i, v; count[0 .. n])
            if (v != 1) {
                r = cast(ubyte) i;
                break;
            }
        perm = applyMask(perm, r + 1, nextPermMask(r + 1)); // fixme
        count[r] -= 1;
        foreach(i, ref v; count[1 .. r])
            v = cast(ubyte) (i + 2);
        parity ^= 1;
    }
    writefln("%d\nPfannkuchen(%d) = %d\n", checksum, n, maxFlipCount);
}

