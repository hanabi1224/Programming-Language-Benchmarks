// port from 2-i.rs
// pshufb - check if working and report about lack of documentation
@safe:

import std;
import core.simd;

version(LDC) {
    import ldc.simd;
   
}

immutable vSize = 16;
alias vItem = ubyte;
alias Vec = vItem[vSize];

enum NEXT_PERM_MASKS = nextPermMasks();
enum REVERSE_MASKS = reverseMasks();

Vec shuffle(Vec a, Vec mask) {
    Vec r;
    foreach (i; 0 .. vSize)
        r[i] = a[mask[i]]; // mb need cast
    return r;
}

version(LDC) {
    ubyte16 simd_shuffle_rev(ubyte16 a, int idx) {
        mixin("enum mask = AliasSeq!(" ~ format("%(%s,%)", REVERSE_MASKS[idx]) ~ ");");
        return shufflevector!(int4, mask)(a, a);
    }

    ubyte16 simd_shuffle_perm(ubyte16 a, int idx) {
        mixin("enum mask = AliasSeq!(" ~ format("%(%s,%)", NEXT_PERM_MASKS[idx]) ~ ");");
        return shufflevector!(int4, mask)(a, a);
    }
}


Vec reverseMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 0u .. n)
        v.array[i] = cast(ubyte) (n - i - 1u);
    return v;
}

Vec rotateMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 0u .. n)
        v.array[i] = (i + 1u) % n;
    return v;
}

Vec[vSize] reverseMasks() {
    Vec[vSize] v;
    foreach(ubyte i; 0u .. vSize)
        v[i] = reverseMask(i);
    return v;
}

Vec nextPermMask(ubyte n) {
    Vec v = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    foreach(ubyte i; 2u .. n + 1u)
        v = shuffle(v, rotateMask(i));
    return v;
}

Vec[vSize] nextPermMasks() {
    Vec[vSize] v;
    foreach(ubyte i; 0u .. vSize)
        v[i] = nextPermMask(i);
    return v;
}

version(LDC) {
    uint pfannkuchen(ubyte16 perm) {
        uint flipCount = 0u;
        auto a = perm;
        while (true) {
            const k = extractelement!(ubyte16, 0)(a);
            if (k == 0)
                return flipCount;
            a = simd_shuffle_rev(a, k+1);
            flipCount += 1;
        }
    }
}
else {
    uint pfannkuchen(ubyte16 perm) {
        uint flipCount = 0u;
        auto a = perm.array;
        while (true) {
            const k = a[0];
            if (k == 0u)
                return flipCount;
            a = shuffle(a, REVERSE_MASKS[k+1]);
            flipCount += 1u;
        }
    }
}

void main(string[] args) {
    import core.simd: ubyte16;

    const n = args[1].to!ubyte;
    uint maxFlipCount = 0u;
    int checksum = 0;
    ubyte16 perm = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];
    Vec count = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
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
        version(LDC) {
            perm = simd_shuffle_perm(perm, r + 1);
        }
        else {
            perm = cast(ubyte16) shuffle(perm.array, nextPermMask(cast(ubyte) (r + 1u)));
        }
        
        count[r] -= 1;
        ubyte i = 1;
        while (i < r) {
            count[i] = cast(ubyte) (i + 1u); // in 2.zig i + 2
            i++;
        }
        parity ^= 1;
    }
    writefln("%d\nPfannkuchen(%d) = %d\n", checksum, n, maxFlipCount);
}

