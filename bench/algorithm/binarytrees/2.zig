// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// binarytrees in Zig.
// Targets Zig 0.16.0.
// Author: JK
// Build: zig build-exe -O ReleaseFast 2.zig

const std = @import("std");
const Io = std.Io;

const MIN_DEPTH: usize = 4;
const MAX_BANDS = 32;
const MAX_TASKS = 256;
const CHUNK_TARGET: usize = 8;
const BAND_MEMORY_BUDGET: usize = 192 << 20; // 192 MiB
const Node = struct {
    left: ?*Node,
    right: ?*Node,

    fn check(self: *const Node) usize {
        var sum: usize = 1;
        if (self.left) |l| sum += l.check();
        if (self.right) |r| sum += r.check();
        return sum;
    }
};

fn nodeCount(depth: usize) usize {
    return (@as(usize, 2) << @as(u6, @intCast(depth))) - 1;
}

fn iterationsFor(depth: usize, max_depth: usize) usize {
    return @as(usize, 1) << @as(u6, @intCast(max_depth - depth + MIN_DEPTH));
}

const Pool = struct {
    slab: []Node,
    idx: usize = 0,

    fn init(gpa: std.mem.Allocator, capacity: usize) !Pool {
        return .{ .slab = try gpa.alloc(Node, capacity) };
    }

    fn deinit(self: *Pool, gpa: std.mem.Allocator) void {
        gpa.free(self.slab);
    }

    inline fn create(self: *Pool) *Node {
        const n = &self.slab[self.idx];
        self.idx += 1;
        return n;
    }

    inline fn rewind(self: *Pool, mark: usize) void {
        self.idx = mark;
    }

    fn make(self: *Pool, depth: usize) *Node {
        const node = self.create();
        if (depth > 0) {
            node.left = self.make(depth - 1);
            node.right = self.make(depth - 1);
        } else {
            node.left = null;
            node.right = null;
        }
        return node;
    }
};

fn runChunk(pool: *Pool, depth: usize, iterations: usize, out: *usize) void {
    var sum: usize = 0;
    var i: usize = 0;
    while (i < iterations) : (i += 1) {
        pool.rewind(0);
        sum += pool.make(depth).check();
    }
    out.* = sum;
}

fn chunksFor(depth: usize, iterations: usize, per_band_budget: usize) usize {
    const slab_bytes = nodeCount(depth) * @sizeOf(Node);
    const max_by_memory = @max(1, per_band_budget / slab_bytes);
    return @min(@min(CHUNK_TARGET, iterations), max_by_memory);
}

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;

    const n = try parseN(init);
    const max_depth = @max(MIN_DEPTH + 2, n);
    const stretch_depth = max_depth + 1;

    var stdout_buffer: [4096]u8 = undefined;
    var file_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &file_writer.interface;

    {
        var pool: Pool = try .init(gpa, nodeCount(stretch_depth));
        defer pool.deinit(gpa);
        const stretch = pool.make(stretch_depth);
        try stdout.print("stretch tree of depth {d}\t check: {d}\n", .{ stretch_depth, stretch.check() });
    }

    var long_pool: Pool = try .init(gpa, nodeCount(max_depth));
    defer long_pool.deinit(gpa);
    const long_lived = long_pool.make(max_depth);

    const band_count = (max_depth - MIN_DEPTH) / 2 + 1;
    std.debug.assert(band_count <= MAX_BANDS);
    const per_band_budget = BAND_MEMORY_BUDGET / band_count;

    var pools: [MAX_TASKS]Pool = undefined;
    var results: [MAX_TASKS]usize = @splat(0);
    var task_band: [MAX_TASKS]usize = @splat(0);
    var task_count: usize = 0;
    defer for (pools[0..task_count]) |*p| p.deinit(gpa);

    var group: Io.Group = .init;
    defer group.cancel(io);

    var b: usize = band_count;
    while (b > 0) {
        b -= 1;
        const depth = MIN_DEPTH + 2 * b;
        const iterations = iterationsFor(depth, max_depth);
        const chunks = chunksFor(depth, iterations, per_band_budget);

        const base = iterations / chunks;
        const extra = iterations % chunks;

        var c: usize = 0;
        while (c < chunks) : (c += 1) {
            const my_iters = base + if (c < extra) @as(usize, 1) else 0;
            std.debug.assert(task_count < MAX_TASKS);
            pools[task_count] = try .init(gpa, nodeCount(depth));
            task_band[task_count] = b;
            group.async(io, runChunk, .{ &pools[task_count], depth, my_iters, &results[task_count] });
            task_count += 1;
        }
    }

    try group.await(io);

    var band_sums: [MAX_BANDS]usize = @splat(0);
    for (0..task_count) |t| band_sums[task_band[t]] += results[t];

    var band: usize = 0;
    while (band < band_count) : (band += 1) {
        const depth = MIN_DEPTH + 2 * band;
        try stdout.print("{d}\t trees of depth {d}\t check: {d}\n", .{ iterationsFor(depth, max_depth), depth, band_sums[band] });
    }

    try stdout.print("long lived tree of depth {d}\t check: {d}\n", .{ max_depth, long_lived.check() });
    try stdout.flush(); // if this errors, use: try file_writer.flush();
}

fn parseN(init: std.process.Init) !usize {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len < 2) return 10;
    return std.fmt.parseInt(usize, args[1], 10) catch 10;
}
