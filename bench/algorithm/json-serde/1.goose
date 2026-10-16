// The documents are read with json's reader straight into typed records, as
// the serde derives of the Rust programs read them, and written back with
// json's writer. Records are fixed-size and kept in arenas: a feature's
// polygon is a run of rings, a ring a run of points, and a document a run of
// features, each run a start and a count in the arena below it. Strings are
// views of the text, or of the decoded text for those that held escapes.

import std;
import os;
import json;

struct Run { first: i64, count: i64 }
struct Properties { name: const u8[:] }
struct Geometry { kind: const u8[:], coordinates: Run }    // rings
struct Feature { kind: const u8[:], properties: Properties, geometry: Geometry }
struct GeoData { kind: const u8[:], features: Run }

// --- numbers --------------------------------------------------------------------

// As the Rust programs' MyF64 writes a number: integral values as an i64
// (saturated, as Rust's `as` is), others as json writes them.
fn format_num(out: u8[>..]&, x: f64) {
    if x != x || x - x != 0.0 { json::format_number(out, x); return; }
    if x >= 9223372036854775807.0 { format(out, 9223372036854775807); return; }
    if x <= -9223372036854775808.0 { format(out, -9223372036854775807 - 1); return; }
    let i = x as! i64;
    if i == x { format(out, i); return; }
    json::format_number(out, x);
}

// --- MD5 (RFC 1321) ---------------------------------------------------------------

// G's two terms have no bits in common, so they can be added, which lets
// the one without b be summed before b is known.
inline fn rotl(x: u32, s: u32) -> u32 { (x << s) | (x >> (32 - s)) }
inline fn ff(a: u32, b: u32, c: u32, d: u32, m: u32, s: u32, k: u32) -> u32 { b + rotl(a + (d ^ (b & (c ^ d))) + m + k, s) }
inline fn gg(a: u32, b: u32, c: u32, d: u32, m: u32, s: u32, k: u32) -> u32 { b + rotl(a + m + k + (c & ~d) + (b & d), s) }
inline fn hh(a: u32, b: u32, c: u32, d: u32, m: u32, s: u32, k: u32) -> u32 { b + rotl(a + (b ^ c ^ d) + m + k, s) }
inline fn ii(a: u32, b: u32, c: u32, d: u32, m: u32, s: u32, k: u32) -> u32 { b + rotl(a + (c ^ (b | ~d)) + m + k, s) }

// One 64-byte block into the state h.
fn md5_block(h: u32[4]&, blk: u8[:]) {
    assert(blk.len == 64);
    var m: u32[16] = [0; 16];
    for j in 16 {
        m[j] = blk[4 * j] | (blk[4 * j + 1] as u32) << 8 | (blk[4 * j + 2] as u32) << 16 | (blk[4 * j + 3] as u32) << 24;
    }
    var a = h[0];
    var b = h[1];
    var c = h[2];
    var d = h[3];
    a = ff(a, b, c, d, m[0], 7, 0xd76aa478);    d = ff(d, a, b, c, m[1], 12, 0xe8c7b756);
    c = ff(c, d, a, b, m[2], 17, 0x242070db);   b = ff(b, c, d, a, m[3], 22, 0xc1bdceee);
    a = ff(a, b, c, d, m[4], 7, 0xf57c0faf);    d = ff(d, a, b, c, m[5], 12, 0x4787c62a);
    c = ff(c, d, a, b, m[6], 17, 0xa8304613);   b = ff(b, c, d, a, m[7], 22, 0xfd469501);
    a = ff(a, b, c, d, m[8], 7, 0x698098d8);    d = ff(d, a, b, c, m[9], 12, 0x8b44f7af);
    c = ff(c, d, a, b, m[10], 17, 0xffff5bb1);  b = ff(b, c, d, a, m[11], 22, 0x895cd7be);
    a = ff(a, b, c, d, m[12], 7, 0x6b901122);   d = ff(d, a, b, c, m[13], 12, 0xfd987193);
    c = ff(c, d, a, b, m[14], 17, 0xa679438e);  b = ff(b, c, d, a, m[15], 22, 0x49b40821);

    a = gg(a, b, c, d, m[1], 5, 0xf61e2562);    d = gg(d, a, b, c, m[6], 9, 0xc040b340);
    c = gg(c, d, a, b, m[11], 14, 0x265e5a51);  b = gg(b, c, d, a, m[0], 20, 0xe9b6c7aa);
    a = gg(a, b, c, d, m[5], 5, 0xd62f105d);    d = gg(d, a, b, c, m[10], 9, 0x02441453);
    c = gg(c, d, a, b, m[15], 14, 0xd8a1e681);  b = gg(b, c, d, a, m[4], 20, 0xe7d3fbc8);
    a = gg(a, b, c, d, m[9], 5, 0x21e1cde6);    d = gg(d, a, b, c, m[14], 9, 0xc33707d6);
    c = gg(c, d, a, b, m[3], 14, 0xf4d50d87);   b = gg(b, c, d, a, m[8], 20, 0x455a14ed);
    a = gg(a, b, c, d, m[13], 5, 0xa9e3e905);   d = gg(d, a, b, c, m[2], 9, 0xfcefa3f8);
    c = gg(c, d, a, b, m[7], 14, 0x676f02d9);   b = gg(b, c, d, a, m[12], 20, 0x8d2a4c8a);

    a = hh(a, b, c, d, m[5], 4, 0xfffa3942);    d = hh(d, a, b, c, m[8], 11, 0x8771f681);
    c = hh(c, d, a, b, m[11], 16, 0x6d9d6122);  b = hh(b, c, d, a, m[14], 23, 0xfde5380c);
    a = hh(a, b, c, d, m[1], 4, 0xa4beea44);    d = hh(d, a, b, c, m[4], 11, 0x4bdecfa9);
    c = hh(c, d, a, b, m[7], 16, 0xf6bb4b60);   b = hh(b, c, d, a, m[10], 23, 0xbebfbc70);
    a = hh(a, b, c, d, m[13], 4, 0x289b7ec6);   d = hh(d, a, b, c, m[0], 11, 0xeaa127fa);
    c = hh(c, d, a, b, m[3], 16, 0xd4ef3085);   b = hh(b, c, d, a, m[6], 23, 0x04881d05);
    a = hh(a, b, c, d, m[9], 4, 0xd9d4d039);    d = hh(d, a, b, c, m[12], 11, 0xe6db99e5);
    c = hh(c, d, a, b, m[15], 16, 0x1fa27cf8);  b = hh(b, c, d, a, m[2], 23, 0xc4ac5665);

    a = ii(a, b, c, d, m[0], 6, 0xf4292244);    d = ii(d, a, b, c, m[7], 10, 0x432aff97);
    c = ii(c, d, a, b, m[14], 15, 0xab9423a7);  b = ii(b, c, d, a, m[5], 21, 0xfc93a039);
    a = ii(a, b, c, d, m[12], 6, 0x655b59c3);   d = ii(d, a, b, c, m[3], 10, 0x8f0ccc92);
    c = ii(c, d, a, b, m[10], 15, 0xffeff47d);  b = ii(b, c, d, a, m[1], 21, 0x85845dd1);
    a = ii(a, b, c, d, m[8], 6, 0x6fa87e4f);    d = ii(d, a, b, c, m[15], 10, 0xfe2ce6e0);
    c = ii(c, d, a, b, m[6], 15, 0xa3014314);   b = ii(b, c, d, a, m[13], 21, 0x4e0811a1);
    a = ii(a, b, c, d, m[4], 6, 0xf7537e82);    d = ii(d, a, b, c, m[11], 10, 0xbd3af235);
    c = ii(c, d, a, b, m[2], 15, 0x2ad7d2bb);   b = ii(b, c, d, a, m[9], 21, 0xeb86d391);

    h[0] += a;
    h[1] += b;
    h[2] += c;
    h[3] += d;
}

// The digest of data, in lower-case hex.
fn md5_hex(data: u8[:]) -> u8[>..] {
    var h: u32[4] = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476];
    let full = data.len / 64 * 64;
    var at = 0;
    while at < full {
        md5_block(h, data[at..at + 64]);
        at += 64;
    }
    // The rest, a 0x80, zeros up to 8 bytes short of a block, the length in bits.
    var tail: u8[128] = [0; 128];
    let rest = data.len - full;
    copy_into(tail[..rest], data[full..]);
    tail[rest] = 0x80;
    let end = if rest < 56 { 64 } else { 128 };
    let bits = (data.len as u64) * 8;
    for j in 8 { tail[end - 8 + j] = (bits >> (8 * j)) as! u8; }
    md5_block(h, tail[..64]);
    if end == 128 { md5_block(h, tail[64..]); }
    var out: u8[>..] = [];
    for w in h {
        for j in 4 { format_uint(out, (w >> (8 * j)) & 0xff, 16, 2, '0'); }
    }
    return out;
}

// --- main -------------------------------------------------------------------------

fn main() {
    let argv = args();
    var path: u8[>..] = [];
    path.append(if argv.len > 1 { argv[1] } else { "sample" });
    path.append(".json");
    var n = 10;
    if argv.len > 2 { let v, ok = parse_int(argv[2]); if ok { n = v; } }
    var text: u8[>..] = [];
    if !read_file(path, text) { abort("cannot read the input"); }

    var decoded: u8[>..] = [];      // strings that held escapes
    // The strings read are views of the reader's text, so the reader is
    // one that outlives the records; each document resets it.
    var r = json::reader(text);
    var points: f64[>..] = [];      // x and y, by pairs
    var rings: Run[>..] = [];       // runs of points
    var features: Feature[>..] = [];
    var docs: GeoData[>..] = [];

    fn read_ring(r: json::Reader&) -> Run {
        let first = points.len / 2;
        json::each_element(r) {
            var k = 0;
            json::each_element(r) {
                if k < 2 { points.push(json::read_number(r)); } else { json::skip(r); }
                k++;
            };
            if k != 2 { abort("a point is two numbers"); }
        };
        return Run { first: first, count: points.len / 2 - first };
    }

    fn read_geometry(r: json::Reader&) -> Geometry {
        var g = Geometry { kind: "", coordinates: Run { 0, 0 } };
        json::each_member(r) { key =>
            if key == "type" {
                g.kind = json::read_string(r, decoded);
            } else if key == "coordinates" {
                let first = rings.len;
                json::each_element(r) { rings.push(read_ring(r)); };
                g.coordinates = Run { first: first, count: rings.len - first };
            } else {
                json::skip(r);
            }
        };
        return g;
    }

    fn read_feature(r: json::Reader&) -> Feature {
        var f = Feature { kind: "", properties: Properties { name: "" }, geometry: Geometry { kind: "", coordinates: Run { 0, 0 } } };
        json::each_member(r) { key =>
            if key == "type" {
                f.kind = json::read_string(r, decoded);
            } else if key == "properties" {
                json::each_member(r) { pkey =>
                    if pkey == "name" { f.properties.name = json::read_string(r, decoded); } else { json::skip(r); }
                };
            } else if key == "geometry" {
                f.geometry = read_geometry(r);
            } else {
                json::skip(r);
            }
        };
        return f;
    }

    fn read_doc() -> GeoData {
        r = json::reader(text);
        var d = GeoData { kind: "", features: Run { 0, 0 } };
        json::each_member(r) { key =>
            if key == "type" {
                d.kind = json::read_string(r, decoded);
            } else if key == "features" {
                let first = features.len;
                json::each_element(r) { features.push(read_feature(r)); };
                d.features = Run { first: first, count: features.len - first };
            } else {
                json::skip(r);
            }
        };
        if !json::finish(r) { abort(json::message(r)); }
        return d;
    }

    fn write_doc(out: u8[>..]&, d: GeoData) {
        out.append("{\"type\":");
        json::format_string(out, d.kind);
        out.append(",\"features\":[");
        for fi in d.features.count {
            let f = features[d.features.first + fi];
            if fi > 0 { out.push(','); }
            out.append("{\"type\":");
            json::format_string(out, f.kind);
            out.append(",\"properties\":{\"name\":");
            json::format_string(out, f.properties.name);
            out.append("},\"geometry\":{\"type\":");
            json::format_string(out, f.geometry.kind);
            out.append(",\"coordinates\":[");
            let c = f.geometry.coordinates;
            for ri in c.count {
                let ring = rings[c.first + ri];
                if ri > 0 { out.push(','); }
                out.push('[');
                for pi in ring.count {
                    let at = 2 * (ring.first + pi);
                    if pi > 0 { out.push(','); }
                    out.push('[');
                    format_num(out, points[at]);
                    out.push(',');
                    format_num(out, points[at + 1]);
                    out.push(']');
                }
                out.push(']');
            }
            out.append("]}}");
        }
        out.append("]}");
    }

    var out: u8[>..] = [];
    let first = read_doc();
    write_doc(out, first);
    print(md5_hex(out));

    for i in n { docs.push(read_doc()); }
    var all: u8[>..] = [];
    all.push('[');
    for d, i in docs {
        if i > 0 { all.push(','); }
        write_doc(all, d);
    }
    all.push(']');
    print(md5_hex(all));
}
