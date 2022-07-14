// From https://github.com/tiehuis/zig-benchmarks-game/blob/master/src/fannkuch-redux.zig

const std = @import("std");

const global_allocator = std.heap.c_allocator;

var buffer: [1024]u8 = undefined;
var fixed_allocator = std.heap.FixedBufferAllocator.init(buffer[0..]);
var allocator = fixed_allocator.allocator();

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();

    var perm = try allocator.alloc(usize, n);
    var perm1 = try allocator.alloc(usize, n);
    var count = try allocator.alloc(usize, n);

    var max_flips_count: usize = 0;
    var perm_count: usize = 0;
    var checksum: isize = 0;

    for (perm1) |*e, i| {
        e.* = i;
    }

    var r = n;
    loop: {
        while (true) {
            while (r != 1) : (r -= 1) {
                count[r - 1] = r;
            }

            for (perm) |_, i| {
                perm[i] = perm1[i];
            }

            var flips_count: usize = 0;

            while (true) {
                const k = perm[0];
                if (k == 0) {
                    break;
                }

                const k2 = (k + 1) >> 1;
                var i: usize = 0;
                while (i < k2) : (i += 1) {
                    std.mem.swap(usize, &perm[i], &perm[k - i]);
                }
                flips_count += 1;
            }

            max_flips_count = std.math.max(max_flips_count, flips_count);
            if (perm_count % 2 == 0) {
                checksum += @intCast(isize, flips_count);
            } else {
                checksum -= @intCast(isize, flips_count);
            }

            while (true) : (r += 1) {
                if (r == n) {
                    break :loop;
                }

                const perm0 = perm1[0];
                var i: usize = 0;
                while (i < r) {
                    const j = i + 1;
                    perm1[i] = perm1[j];
                    i = j;
                }

                perm1[r] = perm0;
                count[r] -= 1;

                if (count[r] > 0) {
                    break;
                }
            }

            perm_count += 1;
        }
    }

    try stdout.print("{d}\nPfannkuchen({d}) = {d}\n", .{ checksum, n, max_flips_count });
}

fn get_n() !usize {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        return try std.fmt.parseInt(usize, args[1], 10);
    } else {
        return 10;
    }
}
