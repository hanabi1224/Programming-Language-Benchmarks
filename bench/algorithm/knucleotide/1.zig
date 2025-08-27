const std = @import("std");
const print = @import("print.zig");

const global_allocator = std.heap.c_allocator;


const Code = struct {
    data: u64,

    pub inline fn encodeByte(c: u8) u8 {
        return (c >> 1) & 0b11;
    }

    pub inline fn makeMask(frame: usize) u64 {
        return (@as(u64, 1) << @as(u6, @intCast(2 * frame))) - 1;
    }

    pub inline fn push(self: *Code, c: u8, mask: u64) void {
        self.data = ((self.data << 2) | c) & mask;
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
        var result: std.ArrayList(u8) = .{};
        var code = self.data;
        var i: usize = 0;
        while (i < frame) : (i += 1) {
            const c: u8 = switch (@as(u8, @truncate(code)) & 0b11) {
                Code.encodeByte('A') => 'A',
                Code.encodeByte('T') => 'T',
                Code.encodeByte('G') => 'G',
                Code.encodeByte('C') => 'C',
                else => unreachable,
            };
            try result.append(global_allocator, c);
            code >>= 2;
        }
        std.mem.reverse(u8, result.items);
        return result.toOwnedSlice(global_allocator);
    }
};

pub fn readInput() ![]const u8 {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    const file_name = if (args.len > 1) args[1] else "25000_in";
    var file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();
    var whole = try file.readToEndAlloc(global_allocator, std.math.maxInt(u32));
    // find after third header line starting with '>' then the end of that line
    var gt_count: u8 = 0;
    var i: usize = 0;
    while (i < whole.len and gt_count < 3) : (i += 1) {
        if (whole[i] == '>') {
            gt_count += 1;
        }
    }
    // move i to end of current line
    while (i < whole.len and whole[i] != '\n') : (i += 1) {}
    if (i < whole.len) i += 1; // skip newline
    var buf = whole[i..];
    // In place, remove all newlines from buf and encode nucleotides
    // using only the last 2 bits in each byte.
    {
        var j: usize = 0;
        for (buf) |c| {
            if (c != '\n') {
                // Gives a -> 0x00, c -> 0x01, g -> 0x03, t -> 0x02
                buf[j] = (c >> 1) & 0x03;
                j += 1;
            }
        }
        buf.len = j;
    }
    return buf;
}

const CodeContext = struct {
    pub fn eql(_: CodeContext, a: Code, b: Code) bool { return a.data == b.data; }
    pub fn hash(_: CodeContext, c: Code) u64 { return c.data ^ (c.data >> 7); }
};

const Map = std.HashMapUnmanaged(Code, u32, CodeContext, 45);

const Iter = struct {
    i: usize = 0,
    input: []const u8,
    code: Code,
    mask: u64,

    pub fn init(input: []const u8, frame: usize) Iter {
        const mask = Code.makeMask(frame);
        var code = Code{ .data = 0 };
        for (input[0 .. frame - 1]) |c| code.push(c, mask);
        return .{
            .input = input[frame - 1 ..],
            .code = code,
            .mask = mask,
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

fn genMap(seq: []const u8, n: usize, map: *Map) !void {
    map.clearAndFree(global_allocator);
    var iter = Iter.init(seq, n);
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
        return order == .lt or (order == .eq and b.code.data < a.code.data);
    }
};

fn printMap(self: usize, map: Map) !void {
    var v: std.ArrayList(CountCode) = .{};
    defer v.deinit(global_allocator);
    var iter = map.iterator();
    var total: u64 = 0;
    while (iter.next()) |it| {
        const count = it.value_ptr.*;
        total += count;
    try v.append(global_allocator, .{ .count = count, .code = it.key_ptr.* });
    }

    std.mem.sort(CountCode, v.items, {}, comptime CountCode.asc);
    var i = v.items.len - 1;
    while (true) : (i -= 1) {
        const cc = v.items[i];
        try print.printFmt("{!s} {d:.3}\n", .{
            cc.code.toString(self),
            @as(f32, @floatFromInt(cc.count)) / @as(f32, @floatFromInt(total)) * 100.0,
        });
        if (i == 0) break;
    }
    try print.printFmt("\n", .{});
}

fn printOcc(s: []const u8, map: *Map) !void {
    const count = if (map.get(Code.fromStr(s))) |x| x else 0;
    try print.printFmt("{}\t{s}\n", .{ count, s });
}

pub fn main() !void {
    const occs = [_][]const u8{
        "GGT",
        "GGTA",
        "GGTATT",
        "GGTATTTTAATT",
        "GGTATTTTAATTTATAGT",
    };
    const input = try readInput();
    var map: Map = .{};
    try genMap(input, 1, &map);
    try printMap(1, map);
    try genMap(input, 2, &map);
    try printMap(2, map);

    for (occs) |occ| {
        try genMap(input, occ.len, &map);
        try printOcc(occ, &map);
    }
}
