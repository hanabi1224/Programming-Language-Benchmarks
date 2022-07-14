const std = @import("std");

const vec4 = std.meta.Vector(4, f64);
fn vec1to4(f: f64) vec4 {
    return @splat(4, f);
}

fn runInParallel(tasks: []std.Thread, len: usize, comptime f: anytype, args: anytype) !void {
    const len_per_task = @divTrunc(len, tasks.len + 1);
    for (tasks) |*task, i| {
        const first = len_per_task * i;
        const last = first + len_per_task;
        task.* = try std.Thread.spawn(.{}, f, .{ first, last } ++ args);
    }
    @call(.{}, f, .{ tasks.len * len_per_task, len } ++ args);
    for (tasks) |*task| task.join();
}

fn baseIdx(i: vec4) vec4 {
    @setFloatMode(.Optimized);
    return i * (i + vec1to4(1)) * vec1to4(0.5) + vec1to4(1);
}

fn multAvGeneric(comptime transpose: bool, first: usize, dst: []vec4, src: []const vec4) void {
    @setFloatMode(.Optimized);
    const srcVals = std.mem.bytesAsSlice(f64, std.mem.sliceAsBytes(src));
    var ti = vec1to4(@intToFloat(f64, first * 4)) + if (transpose) vec4{ 1, 2, 3, 4 } else vec4{ 0, 1, 2, 3 };
    for (dst) |*res| {
        var idx = if (transpose) baseIdx(ti - vec1to4(1)) else baseIdx(ti) + ti;
        var sum = vec1to4(0);
        for (srcVals) |u, j| {
            sum += vec1to4(u) / idx;
            idx += ti + vec1to4(@intToFloat(f64, j + 1));
        }
        res.* = sum;
        ti += vec1to4(4);
    }
}

fn multAv(first: usize, last: usize, dst: []vec4, src: []const vec4) void {
    return multAvGeneric(false, first, dst[first..last], src);
}

fn multAtv(first: usize, last: usize, dst: []vec4, src: []const vec4) void {
    return multAvGeneric(true, first, dst[first..last], src);
}

fn multAtAv(tasks: []std.Thread, dest: []vec4, src: []const vec4, temp: []vec4) !void {
    std.debug.assert(dest.len == src.len and src.len == temp.len);
    try runInParallel(tasks, dest.len, multAv, .{ temp, src });
    try runInParallel(tasks, dest.len, multAtv, .{ dest, temp });
}

fn setOnes(first: usize, last: usize, dst: []vec4) void {
    for (dst[first..last]) |*v| v.* = vec1to4(1);
}

fn aggregateResults(first: usize, last: usize, u: []const vec4, v: []const vec4, total_vbv: *f64, total_vv: *f64) void {
    @setFloatMode(.Optimized);
    var vbv = vec1to4(0);
    var vv = vec1to4(0);
    for (v[first..last]) |f, i| {
        vbv += u[first + i] * f;
        vv += f * f;
    }
    _ = @atomicRmw(f64, total_vbv, .Add, @reduce(.Add, vbv), .SeqCst);
    _ = @atomicRmw(f64, total_vv, .Add, @reduce(.Add, vv), .SeqCst);
}

pub fn main() !void {
    const n = try get_n();
    const len = n / @typeInfo(vec4).Vector.len;

    const allocator = std.heap.c_allocator;
    var u = try allocator.alloc(vec4, len);
    defer allocator.free(u);
    var v = try allocator.alloc(vec4, len);
    defer allocator.free(v);
    var temp = try allocator.alloc(vec4, len);
    defer allocator.free(temp);

    const task_count = try std.Thread.getCpuCount();
    const tasks = try allocator.alloc(std.Thread, task_count - 1);
    defer allocator.free(tasks);

    try runInParallel(tasks, u.len, setOnes, .{u});

    var times: usize = 0;
    while (times < 10) : (times += 1) {
        try multAtAv(tasks, v, u, temp);
        try multAtAv(tasks, u, v, temp);
    }

    var vbv: f64 = 0;
    var vv: f64 = 0;
    try runInParallel(tasks, u.len, aggregateResults, .{ u, v, &vbv, &vv });
    const res = std.math.sqrt(vbv / vv);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d:.9}\n", .{res});
}

fn get_n() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 100;
    return try std.fmt.parseInt(usize, arg, 10);
}
