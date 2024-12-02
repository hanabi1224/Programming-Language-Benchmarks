const std = @import("std");

const max_n = 12;
const Vec = @Vector(max_n, u8);

fn runInParallel(tasks: []std.Thread, len: usize, comptime f: anytype, args: anytype) !void {
    const len_per_task = @divTrunc(len, tasks.len + 1);
    for (tasks, 0..) |*task, i| {
        const first = len_per_task * i;
        const last = first + len_per_task;
        task.* = try std.Thread.spawn(.{}, f, .{ first, last } ++ args);
    }
    @call(.auto, f, .{ tasks.len * len_per_task, len } ++ args);
    for (tasks) |*task| task.join();
}

fn reverse_mask(n: u8) Vec {
    // global constant is not used to workaround a compiler bug
    var v = Vec{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    var i: u8 = 0;
    while (i < n) : (i += 1) v[i] = n - i - 1;
    return v;
}

fn rotate_mask(n: u8) Vec {
    var v = Vec{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    var i: u8 = 0;
    while (i < n) : (i += 1) v[i] = (i + 1) % n;
    return v;
}

fn nextPermMask(n: u8) Vec {
    var v = Vec{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    var i: u8 = 2;
    while (i <= n) : (i += 1) v = applyMask(v, i, rotate_mask);
    return v;
}

fn applyMask(a: Vec, n: u8, comptime mask: anytype) Vec {
    comptime var i: u8 = 0;
    inline while (i < max_n) : (i += 1) if (i == n) return @shuffle(u8, a, undefined, mask(i));
    unreachable;
}

fn pfannkuchen(perm: Vec) u32 {
    var flip_count: u32 = 0;
    var a = perm;
    while (true) {
        const k = a[0];
        if (k == 0) return flip_count;
        a = applyMask(a, k + 1, reverse_mask);
        flip_count += 1;
    }
}

fn factorial(n: u8) u32 {
    var res: u32 = 1;
    var i: u8 = 2;
    while (i <= n) : (i += 1) res *= i;
    return res;
}

fn factorialComptime(n: u8) u32 {
    comptime var i = 0;
    inline while (i < max_n) : (i += 1) if (i == n) return comptime factorial(i);
    unreachable;
}

fn countAtPos(n: u8, start: usize) [max_n]u8 {
    var count: [max_n]u8 = undefined;
    var r = start;
    var i = n;
    while (i > 0) {
        i -= 1;
        const total_perms = factorialComptime(i);
        count[i] = i + 1 - @as(u8, @intCast(r / total_perms));
        r %= total_perms;
    }
    return count;
}

fn permWithCount(n: u8, count: [max_n]u8) Vec {
    var perm = Vec{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    const permVals = std.mem.asBytes(&perm);
    var i = n;
    while (i > 0) : (i -= 1) std.mem.rotate(u8, permVals[0..i], i - count[i - 1]);
    return perm;
}

const Stats = struct {
    checksum: i32 = 0,
    max_flips: u32 = 0,
};

fn nextPermutation(perm: Vec, count: []u8) ?Vec {
    const r = for (count, 0..) |v, i| {
        if (v != 1) break @as(u8, @intCast(i));
    } else return null;
    const next_perm = applyMask(perm, r + 1, nextPermMask);
    count[r] -= 1;
    for (count[0..r], 0..) |*v, i| v.* = @intCast(i + 1);
    return next_perm;
}

fn pfannkuchenStats(first: usize, last: usize, n: u8, res: *Stats) void {
    var count = countAtPos(n, first);
    var perm = permWithCount(n, count);
    var stats = Stats{};
    var i = first;
    while (i < last) : (i += 1) {
        const flips = pfannkuchen(perm);
        const parity = 1 - @as(i32, @intCast(i % 2)) * 2;
        stats.max_flips = @max(stats.max_flips, flips);
        stats.checksum += @as(i32, @intCast(flips)) * parity;
        perm = nextPermutation(perm, count[0..n]) orelse break;
    }
    _ = @atomicRmw(u32, &res.max_flips, .Max, stats.max_flips, .seq_cst);
    _ = @atomicRmw(i32, &res.checksum, .Add, stats.checksum, .seq_cst);
}

pub fn main() !void {
    const n = try get_n();

    var tasks_buf: [64]std.Thread = undefined;
    const task_count = try std.Thread.getCpuCount();
    const tasks = tasks_buf[0 .. task_count - 1];
    var stats = Stats{};
    const perms_count = factorialComptime(n);
    try runInParallel(tasks, perms_count, pfannkuchenStats, .{ n, &stats });

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\nPfannkuchen({d}) = {d}\n", .{ stats.checksum, n, stats.max_flips });
}

fn get_n() !u8 {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u8, arg, 10);
}
