// port of 1.cr
@safe:

import std;

Tuple!(uint, uint) fannkuchredux(ubyte n) {
    static int[32] perm1 = iota(32).array;
    static int[32] perm;
    static int[32] count;
    uint maxFlipsCount, permCount, checksum;
    ubyte r = n;
    while (true) {
        while (r > 1) {
            count[r - 1] = r;
            r--;
        }
        perm[] = perm1[];
        uint flipsCount;
        auto k = perm[0];
        while (k != 0) {
            auto k2 = (k+1) >> 1;
            foreach(i;0..k2) {
                auto j = k - i;
                swap(perm[i],perm[j]);
            }
            flipsCount++;
            k = perm[0];
        }
        maxFlipsCount = flipsCount > maxFlipsCount ? flipsCount : maxFlipsCount;
        checksum += (permCount % 2 == 0) ? flipsCount : -flipsCount;

        while (true) {
            if (n == r)
                return tuple(checksum, maxFlipsCount);
            auto perm0 = perm1[0];
            foreach(i; 0..r) {
                auto j = i + 1;
                swap(perm1[i], perm1[j]);
            }
            perm1[r] = perm0;
            count[r] -= 1;
            if (count[r] > 0)
                break;
            r++;
        }
        permCount++;
    }
}

void main(string[] args) {
    ubyte n = args[1].to!ubyte;
    auto ans = fannkuchredux(n);
    writefln("%d\nPfannkuchen(%d) = %d", ans[0],n,ans[1]);
}
