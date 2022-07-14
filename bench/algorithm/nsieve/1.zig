const std = @import("std");

const global_allocator = std.heap.c_allocator;

fn nsieve(n: usize) !void {
    var count: usize = 0;
    var flags = try global_allocator.alloc(bool, n);
    defer global_allocator.free(flags);
    var i: usize = 2;
    while (i < n) : (i += 1) {
        if (!flags[i]) {
            count += 1;
            var j: usize = i << 1;
            while (j < n) : (j += i) {
                flags[j] = true;
            }
        }
    }
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Primes up to {d:8} {d:8}\n", .{ n, count });
}

pub fn main() !void {
    const n = try get_n();
    var i: u6 = 0;
    while (i < 3) : (i += 1) {
        const base: usize = 10000;
        try nsieve(base << (n - i));
    }
}

fn get_n() !u6 {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        return try std.fmt.parseInt(u6, args[1], 10);
    } else {
        return 4;
    }
}
