const std = @import("std");

const global_allocator = std.heap.c_allocator;
const stdout = std.io.getStdOut().writer();

const Code = struct {
    data: u64,

    pub inline fn encodeByte(c: u8) u8 {
        return (c >> 1) & 0b11;
    }

    pub inline fn makeMask(frame: usize) u64 {
        return (@as(u64, 1) << @intCast(u6, (2 * frame))) - 1;
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
        var result = std.ArrayList(u8).init(global_allocator);
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
            try result.append(c);
            code >>= 2;
        }
        std.mem.reverse(u8, result.items);
        return result.toOwnedSlice();
    }
};

pub fn readInput() ![]const u8 {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    const file_name = if (args.len > 1) args[1] else "25000_in";
    const file = try std.fs.cwd().openFile(file_name, .{});
    var buffered_reader = std.io.bufferedReader(file.reader());
    const reader = buffered_reader.reader();
    { // skip past first lines starting with '>'
        var i: u8 = 0;
        while (i < 3) : (i += 1) {
            while (true) {
                const c = try reader.readByte();
                if (c == '>') break;
            }
        }
        while (true) {
            const c = try reader.readByte();
            if (c == '\n') break;
        }
    }

    var buf = try reader.readAllAlloc(global_allocator, std.math.maxInt(u32));
    // In place, remove all newlines from buf and encode nucleotides
    // using only the last 2 bits in each byte.
    {
        var i: usize = 0;
        for (buf) |c| {
            if (c != '\n') {
                // Gives a -> 0x00, c -> 0x01, g -> 0x03, t -> 0x02
                buf[i] = (c >> 1) & 0x03;
                i += 1;
            }
        }
        buf.len = i;
    }
    return buf;
}

const Map = std.AutoHashMapUnmanaged(Code, u32);
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
    map.clearRetainingCapacity();
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
        try stdout.print("{!s} {d:.3}\n", .{
            cc.code.toString(self),
            @intToFloat(f32, cc.count) / @intToFloat(f32, total) * 100.0,
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
