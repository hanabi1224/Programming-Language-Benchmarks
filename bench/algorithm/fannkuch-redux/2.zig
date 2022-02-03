const std = @import("std");

const Vec = std.meta.Vector(16, u8);

fn reverse_mask(n: u8) Vec {
    // global constant is not used to workaround a compiler bug
    var v = Vec { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    var i: u8 = 0;
    while (i < n) : (i += 1) v[i] = n - i - 1;
    return v;
}

fn rotate_mask(n: u8) Vec {
    var v = Vec { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    var i: u8 = 0;
    while (i < n) : (i += 1) v[i] = (i + 1) % n;
    return v;
}

fn next_perm_mask(n: u8) Vec {
    var v = Vec { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    var i: u8 = 2;
    while (i <= n) : (i += 1) v = apply_mask(v, i, rotate_mask);
    return v;
}

fn apply_mask(a: Vec, n: u8, comptime mask: anytype) Vec {
    const len = @typeInfo(Vec).Vector.len;
    comptime var i: u8 = 0;
    inline while (i < len) : (i += 1) if (i == n) return @shuffle(u8, a, undefined, mask(i));
    unreachable;
}

fn pfannkuchen(perm: Vec) u32 {
    var flip_count: u32 = 0;
    var a = perm;
    while (true) {
        const k = a[0];
        if (k == 0) return flip_count;
        a = apply_mask(a, k + 1, reverse_mask);
        flip_count += 1;
    }
}

pub fn main() !void {
    const n = try get_n();

    var max_flip_count: u32 = 0;
    var checksum: i32 = 0;
    var perm = Vec { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    var count = [_]u8 { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
    var parity: u1 = 0;
    while (true) : (parity +%= 1) {
        const flip_count = pfannkuchen(perm);
        max_flip_count = std.math.max(max_flip_count, flip_count);
        checksum += @intCast(i32, flip_count) * (1 - @intCast(i32, parity)*2);
        const r = for (count[0..n]) |v, i| {
            if (v != 1) break @intCast(u8, i);
        } else break;
        perm = apply_mask(perm, r + 1, next_perm_mask);
        count[r] -= 1;
        for (count[1..r]) |*v, i| v.* = @intCast(u8, i + 2);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\nPfannkuchen({d}) = {d}\n", .{ checksum, n, max_flip_count });
}

fn get_n() !u8 {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u8, arg, 10);
}
