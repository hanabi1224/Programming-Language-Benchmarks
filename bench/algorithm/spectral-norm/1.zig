// From https://github.com/tiehuis/zig-benchmarks-game/blob/master/src/spectral-norm.zig

const std = @import("std");

const global_allocator = std.heap.c_allocator;

fn eval_a(i: usize, j: usize) f64 {
    return 1.0 / @intToFloat(f64, (i + j) * (i + j + 1) / 2 + i + 1);
}

fn eval_a_times_u(comptime transpose: bool, au: []f64, u: []const f64) void {
    for (au) |*e| {
        e.* = 0;
    }

    var i: usize = 0;
    while (i < au.len) : (i += 1) {
        var j: usize = 0;
        var a: f64 = 0.0;
        while (j < au.len) : (j += 1) {
            if (transpose) {
                a += eval_a(j, i) * u[j];
            } else {
                a += eval_a(i, j) * u[j];
            }
        }
        au[i] = a;
    }
}

fn eval_ata_times_u(atau: []f64, u: []const f64, scratch: []f64) void {
    std.debug.assert(atau.len == u.len and u.len == scratch.len);

    eval_a_times_u(false, scratch, u);
    eval_a_times_u(true, atau, scratch);
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();

    var u = try global_allocator.alloc(f64, n);
    var v = try global_allocator.alloc(f64, n);
    var scratch = try global_allocator.alloc(f64, n);

    for (u) |*e| {
        e.* = 1;
    }

    var i: usize = 0;
    while (i < 10) : (i += 1) {
        eval_ata_times_u(v, u, scratch);
        eval_ata_times_u(u, v, scratch);
    }

    var vbv: f64 = 0;
    var vv: f64 = 0;

    var j: usize = 0;
    while (j < n) : (j += 1) {
        vbv += u[i] * v[i];
        vv += v[i] * v[i];
    }

    try stdout.print("{d:.9}\n", .{std.math.sqrt(vbv / vv)});
}

fn get_n() !usize {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        return try std.fmt.parseInt(usize, args[1], 10);
    } else {
        return 100;
    }
}
