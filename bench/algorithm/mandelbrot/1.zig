const std = @import("std");
const ArrayList = std.ArrayList;
const md5 = std.crypto.hash.Md5;

const VEC_SIZE = 8;
const Vec = std.meta.Vector(VEC_SIZE, f64);

const global_allocator = std.heap.c_allocator;

pub fn main() !void {
    const n = try get_n();
    const size = (n + VEC_SIZE - 1) / VEC_SIZE * VEC_SIZE;
    const chunk_size = size / VEC_SIZE;
    const inv = 2.0 / @intToFloat(f64, size);
    var xloc = ArrayList(Vec).init(global_allocator);
    try xloc.ensureTotalCapacityPrecise(chunk_size);
    var i: usize = 0;
    while (i < chunk_size) : (i += 1) {
        const offset = i * VEC_SIZE;
        const v = Vec{
            init_xloc(offset, inv),
            init_xloc(offset + 1, inv),
            init_xloc(offset + 2, inv),
            init_xloc(offset + 3, inv),
            init_xloc(offset + 4, inv),
            init_xloc(offset + 5, inv),
            init_xloc(offset + 6, inv),
            init_xloc(offset + 7, inv),
        };
        try xloc.append(v);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("P4\n{d} {d}\n", .{ size, size });

    var pixels = ArrayList(u8).init(global_allocator);
    try pixels.ensureTotalCapacityPrecise(size * chunk_size);
    var y: usize = 0;
    while (y < size) : (y += 1) {
        const ci = @intToFloat(f64, y) * inv - 1.0;
        var x: usize = 0;
        while (x < chunk_size) : (x += 1) {
            const r = mbrot8(xloc.items[x], ci);
            try pixels.append(r);
        }
    }

    // try stdout.print("{}\n", .{pixels});
    var hash: [16]u8 = undefined;
    md5.hash(pixels.items, &hash, .{});
    try stdout.print("{}\n", .{std.fmt.fmtSliceHexLower(&hash)});
}

fn mbrot8(cr: Vec, civ: f64) u8 {
    const ci = @splat(VEC_SIZE, civ);
    const zero: f64 = 0.0;
    var zr = @splat(VEC_SIZE, zero);
    var zi = @splat(VEC_SIZE, zero);
    var tr = @splat(VEC_SIZE, zero);
    var ti = @splat(VEC_SIZE, zero);
    var absz = @splat(VEC_SIZE, zero);

    var _i: u8 = 0;
    while (_i < 10) : (_i += 1) {
        var _j: u8 = 0;
        while (_j < 5) : (_j += 1) {
            zi = (zr + zr) * zi + ci;
            zr = tr - ti + cr;
            tr = zr * zr;
            ti = zi * zi;
        }
        absz = tr + ti;
        var terminate = true;
        var i: u8 = 0;
        while (i < VEC_SIZE) : (i += 1) {
            if (absz[i] <= 4.0) {
                terminate = false;
                break;
            }
        }
        if (terminate) {
            return 0;
        }
    }
    var accu: u8 = 0;
    var i: u8 = 0;
    while (i < VEC_SIZE) : (i += 1) {
        if (absz[i] <= 4.0) {
            const lhs: u8 = 0x80;
            accu |= (lhs >> @intCast(u3, i));
        }
    }
    return accu;
}

fn init_xloc(i: usize, inv: f64) f64 {
    return @intToFloat(f64, i) * inv - 1.5;
}

fn get_n() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 200;
    return try std.fmt.parseInt(usize, arg, 10);
}
