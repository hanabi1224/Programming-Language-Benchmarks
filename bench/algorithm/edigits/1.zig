const std = @import("std");
const bigint = std.math.big.int;
const math = std.math;
const global_allocator = std.heap.c_allocator;

const Pair = struct {
    p: bigint.Managed,
    q: bigint.Managed,
    pub fn deinit(self: *Pair) void {
        defer self.p.deinit();
        defer self.q.deinit();
    }
};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();
    var k = binary_search(n);
    var pair = try sum_terms(0, k - 1);
    defer pair.deinit();
    var p = pair.p;
    var q = pair.q;

    var answer = try bigint.Managed.init(global_allocator);
    defer answer.deinit();
    try bigint.Managed.add(&answer, &p, &q);
    var a = try bigint.Managed.init(global_allocator);
    defer a.deinit();
    var ten = try bigint.Managed.initSet(global_allocator, 10);
    defer ten.deinit();
    try bigint.Managed.pow(&a, &ten, @bitCast(u32, n - 1));
    var tmp = try bigint.Managed.init(global_allocator);
    defer tmp.deinit();
    try bigint.Managed.mul(&tmp, &answer, &a);
    try bigint.Managed.divFloor(&answer, &a, &tmp, &q);
    var str = try answer.toString(global_allocator, 10, std.fmt.Case.lower);
    var i: usize = 0;
    var n_usize = @as(usize, @bitCast(u32, n));
    while (i < n_usize) {
        var sb = [10:0]u8{ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' };
        var j: usize = i;
        while (j < n_usize and j < i + 10) {
            sb[j - i] = str[j];
            j += 1;
        }
        if (i + 10 <= n_usize) {
            try stdout.print("{s}\t:{d}\n", .{ sb, i + 10 });
        } else {
            try stdout.print("{s}\t:{d}\n", .{ sb, n });
        }
        i += 10;
    }
}

fn sum_terms(a: i32, b: i32) anyerror!Pair {
    if (b == a + 1) {
        var p = try bigint.Managed.initSet(global_allocator, 1);
        var q = try bigint.Managed.initSet(global_allocator, b);
        return Pair{
            .p = p,
            .q = q,
        };
    }
    var mid: i32 = @divFloor((a + b), 2);
    var pair_left: Pair = try sum_terms(a, mid);
    defer pair_left.deinit();
    var pair_right: Pair = try sum_terms(mid, b);
    defer pair_right.deinit();
    var left = try bigint.Managed.init(global_allocator);
    try bigint.Managed.mul(&left, &pair_left.p, &pair_right.q);
    try bigint.Managed.add(&left, &left, &pair_right.p);
    var right = try bigint.Managed.init(global_allocator);
    try bigint.Managed.mul(&right, &pair_left.q, &pair_right.q);
    return Pair{
        .p = left,
        .q = right,
    };
}

fn binary_search(n: i32) i32 {
    var a: i32 = 0;
    var b: i32 = 1;
    while (!test_k(n, b)) {
        a = b;
        b *= 2;
    }
    while (b - a > 1) {
        var m: i32 = @divFloor(a + b, 2);
        if (test_k(n, m)) {
            b = m;
        } else {
            a = m;
        }
    }
    return b;
}

fn test_k(n: i32, k: i32) bool {
    if (k < 0) {
        return false;
    }
    var float_k = @intToFloat(f64, k);
    var float_n = @intToFloat(f64, n);
    var ln_k_factorial = float_k * (math.ln(float_k) - 1.0) + 0.5 * math.ln(math.tau);
    var log_10_k_factorial = ln_k_factorial / math.ln10;
    return log_10_k_factorial >= float_n + 50.0;
}

fn get_n() !i32 {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        return try std.fmt.parseInt(i32, args[1], 10);
    } else {
        return 27;
    }
}
