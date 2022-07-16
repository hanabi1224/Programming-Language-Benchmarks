const std = @import("std");

const global_allocator = std.heap.c_allocator;
const stdout = std.io.getStdOut().writer();

const Code = struct {
    data: u64,

    pub inline fn encodeByte(c: u8) u8 {
        return (c >> 1) & 0b11;
    }

    pub inline fn makeMask(frame: usize) u64 {
        return (@as(u64, 1) << (2 * @intCast(u6, frame))) - 1;
    }

    pub inline fn push(self: *Code, c: u8, mask: u64) void {
        self.data <<= 2;
        self.data |= c;
        self.data &= mask;
    }

    pub fn fromStr(s: []const u8) Code {
        const mask = Code.makeMask(s.len);
        var res = Code{ .data = 0 };
        for (s) |c| {
            res.push(Code.encodeByte(c), mask);
        }
        return res;
    }

    pub fn toString(self: Code, frame: usize) ![]const u8 {
        var res = std.ArrayList(u8).init(global_allocator);
        var code = self.data;
        var i: usize = 0;
        while (i < frame) : (i += 1) {
            const c: u8 = switch (@truncate(u8, code) & 0b11) {
                Code.encodeByte('A') => 'A',
                Code.encodeByte('T') => 'T',
                Code.encodeByte('G') => 'G',
                Code.encodeByte('C') => 'C',
                else => unreachable,
            };
            try res.append(c);
            code >>= 2;
        }
        std.mem.reverse(u8, res.items);
        return res.toOwnedSlice();
    }
};

pub fn readInput() ![]const u8 {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    const file_name = if (args.len > 1) args[1] else "25000_in";
    const file = try std.fs.cwd().openFile(file_name, .{});
    const key = ">THREE";

    const reader = std.io.bufferedReader(file.reader()).reader();
    var linebuf: [64]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&linebuf, '\n')) |line| {
        if (line.len == 0 or std.mem.startsWith(u8, line, key)) break;
    }
    var result = try std.ArrayList(u8).initCapacity(global_allocator, 65536);
    while (try reader.readUntilDelimiterOrEof(&linebuf, '\n')) |line| {
        for (line) |c, i| {
            line[i] = Code.encodeByte(c);
        }
        try result.appendSlice(try global_allocator.dupe(u8, line));
    }
    return result.toOwnedSlice();
}

const HMContext = struct {
    const K = Code;
    pub inline fn hash(_: HMContext, key: K) u64 {
        return key.data;
    }
    pub inline fn eql(_: HMContext, a: K, b: K) bool {
        return a.data == b.data;
    }
};

const Map = std.HashMapUnmanaged(Code, u32, HMContext, std.hash_map.default_max_load_percentage);
const Iter = struct {
    i: usize = 0,
    input: []const u8,
    code: Code,
    mask: u64,

    pub fn init(input: []const u8, frame: usize) Iter {
        return .{
            .input = input,
            .code = Code{ .data = 0 },
            .mask = Code.makeMask(frame),
        };
    }
    pub fn next(self: *Iter) ?Code {
        if (self.i >= self.input.len) return null;
        defer self.i += 1;
        const c = self.input[self.i];
        Code.push(&self.code, c, self.mask);
        return self.code;
    }
};

fn genMap(input: []const u8, frame: usize, map: *Map) !void {
    map.clearRetainingCapacity();
    var iter = Iter.init(input, frame);
    while (iter.next()) |code| {
        const gop = try map.getOrPut(global_allocator, code);
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }
}

const CountCode = struct {
    count: u64,
    code: Code,
    pub fn asc(_: void, a: CountCode, b: CountCode) bool {
        const order = std.math.order(a.count, b.count);
        return order == .lt or (order == .eq and a.code.data < b.code.data);
    }
};

fn printMap(self: usize, map: Map) !void {
    var v = std.ArrayList(CountCode).init(global_allocator);
    defer v.deinit();
    var iter = map.iterator();
    var total: u64 = 0;
    while (iter.next()) |it| {
        const count = it.value_ptr.*;
        total += count;
        try v.append(.{ .count = count, .code = it.key_ptr.* });
    }

    std.sort.sort(CountCode, v.items, {}, comptime CountCode.asc);
    var i = v.items.len - 1;
    while (true) : (i -= 1) {
        const cc = v.items[i];
        try stdout.print("{s} {d:.3}\n", .{
            cc.code.toString(self),
            (@intToFloat(f32, cc.count) * 100.0) / @intToFloat(f32, total),
        });
        if (i == 0) break;
    }
    try stdout.print("\n", .{});
}

fn printOcc(s: []const u8, map: *Map) !void {
    const count = if (map.get(Code.fromStr(s))) |x| x else 0;
    try stdout.print("{}\t{s}\n", .{ count, s });
}

pub fn main() !void {
    const occs = [_][]const u8{
        "GGTATTTTAATTTATAGT",
        "GGTATTTTAATT",
        "GGTATT",
        "GGTA",
        "GGT",
    };
    const input = try readInput();
    var map: Map = .{};
    try genMap(input, 1, &map);
    try printMap(1, map);
    try genMap(input, 2, &map);
    try printMap(2, map);

    var i = occs.len - 1;
    while (true) : (i -= 1) {
        const occ = occs[i];
        try genMap(input, occ.len, &map);
        try printOcc(occ, &map);
        if (i == 0) break;
    }
}
