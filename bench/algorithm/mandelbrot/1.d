@safe:

import std.stdio: writefln;
import std.conv: to;
import std.digest.md;

static immutable VLEN = 8;

version(LDC){
    import core.simd;
    alias Vec = double8;
    
    pragma(inline, true):
    ubyte mbrot8(Vec cr, double civ) {
        immutable Vec ci = civ;
        Vec zr = 0.0;
        Vec zi = 0.0;
        Vec tr = 0.0;
        Vec ti = 0.0;
        Vec absz = 0.0;
        foreach(l; 0 .. 10) {
            foreach(k; 0 .. 5) {
                zi = (zr + zr) * zi + ci;
                zr = tr - ti + cr;
                tr = zr * zr;
                ti = zi * zi;
            }
            absz = tr + ti;
            bool terminate = true;
            foreach(i; 0 .. 8)
                if (absz[i] <= 4.0) {
                    terminate = false;
                    break;
                }
            if (terminate)
                return 0u;
        }
        ubyte accu;
        foreach(i; 0 .. VLEN)
            if (absz[i] <= 4.0) {
                accu |= 0x80 >> i;
            }
        return accu;
    }
}
else {
    alias Vec = double[8];

    pragma(inline, true):
    ubyte mbrot8(Vec cr, double civ) {
        immutable Vec ci = civ;
        Vec zr = 0.0;
        Vec zi = 0.0;
        Vec tr = 0.0;
        Vec ti = 0.0;
        Vec absz = 0.0;
        foreach(l; 0 .. 10) {
            foreach(k; 0 .. 5) {
                zi[] = (zr[] + zr[]) * zi[] + ci[];
                zr[] = tr[] - ti[] + cr[];
                tr[] = zr[] * zr[];
                ti[] = zi[] * zi[];
            }
            absz[] = tr[] + ti[];
            bool terminate = true;
            foreach(i; 0 .. 8)
                if (absz[i] <= 4.0) {
                    terminate = false;
                    break;
                }
            if (terminate)
                return 0u;
        }
        ubyte accu;
        foreach(i; 0 .. VLEN)
            if (absz[i] <= 4.0) {
                accu |= 0x80 >> i;
            }
        return accu;
    }
}

void main(string[] args) {
    auto n = args.length > 1 ? args[1].to!int() : 200;
    auto size = (n + VLEN - 1) / VLEN * VLEN;
    auto chunk_size = size / VLEN;
    writefln("P4\n%d %d", size, size);
    auto inv = 2.0 / size;
    Vec[] xloc = new Vec[chunk_size];
    foreach(i, ref arr; xloc)
        foreach(j, ref el; arr)
            el = (i * 8 + j) * inv - 1.5;

    auto pixels = new ubyte[size * chunk_size];
    double ci;
    foreach(y; 0 .. size) {
        ci = inv * y - 1.0;
        foreach(x; 0 .. chunk_size) {
            auto r = mbrot8(xloc[x], ci);
            pixels[y*chunk_size + x] = r;
        }
    }
    ubyte[16] hash = md5Of(pixels);
    writefln("%s", toHexString!(LetterCase.lower)(hash));
}
