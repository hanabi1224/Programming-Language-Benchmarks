const std = @import("std");

const vec4 = std.meta.Vector(4, f64);
fn vec1to4(f: f64) vec4 {
    return @splat(4, f);
}

fn baseIdx(i: vec4) vec4 {
    @setFloatMode(.Optimized);
    return i * (i + vec1to4(1)) * vec1to4(0.5) + vec1to4(1);
}

fn multAvGeneric(comptime transpose: bool, dst: []vec4, src: []const vec4) void {
    @setFloatMode(.Optimized);
    const srcVals = std.mem.bytesAsSlice(f64, std.mem.sliceAsBytes(src));
    var ti = if (transpose) vec4{ 1, 2, 3, 4 } else vec4{ 0, 1, 2, 3 };
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

fn multAv(dst: []vec4, src: []const vec4) void {
    return multAvGeneric(false, dst, src);
}

fn multAtv(dst: []vec4, src: []const vec4) void {
    return multAvGeneric(true, dst, src);
}

fn multAtAv(dest: []vec4, src: []const vec4, temp: []vec4) void {
    std.debug.assert(dest.len == src.len and src.len == temp.len);
    multAv(temp, src);
    multAtv(dest, temp);
}

fn aggregateResults(u: []const vec4, v: []const vec4) f64 {
    @setFloatMode(.Optimized);
    var vbv = vec1to4(0);
    var vv = vec1to4(0);
    for (v) |f, i| {
        vbv += u[i] * f;
        vv += f * f;
    }
    return std.math.sqrt(@reduce(.Add, vbv) / @reduce(.Add, vv));
}

pub fn main() !void {
    const n = try get_n();
    const len = n / @typeInfo(vec4).Vector.len;

    const allocator = std.heap.c_allocator;

    var u = try allocator.alloc(vec4, len);
    defer allocator.free(u);
    for (u) |*i| i.* = vec1to4(1);

    var v = try allocator.alloc(vec4, len);
    defer allocator.free(v);
    for (v) |*i| i.* = vec1to4(1);

    var temp = try allocator.alloc(vec4, len);
    defer allocator.free(temp);

    const task_count = try std.Thread.getCpuCount();
    const tasks = try allocator.alloc(std.Thread, task_count - 1);
    defer allocator.free(tasks);

    var times: usize = 0;
    while (times < 10) : (times += 1) {
        multAtAv(v, u, temp);
        multAtAv(u, v, temp);
    }

    const res = aggregateResults(u, v);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d:.9}\n", .{res});
}

fn get_n() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 100;
    return try std.fmt.parseInt(usize, arg, 10);
}
