// From https://github.com/tiehuis/zig-benchmarks-game/blob/master/src/fasta.zig

const std = @import("std");

const max_line_length = 60;

const im = 139968;
const ia = 3877;
const ic = 29573;
var seed: u32 = 42;
fn nextRandom(max: f64) f64 {
    seed = (seed * ia + ic) % im;
    return max * @intToFloat(f64, seed) / @intToFloat(f64, im);
}

const AminoAcid = struct {
    l: u8,
    p: f64,
};

fn repeatAndWrap(out: anytype, comptime sequence: []const u8, count: usize) void {
    var padded_sequence: [sequence.len + max_line_length]u8 = undefined;
    for (&padded_sequence, 0..) |*e, i| {
        e.* = sequence[i % sequence.len];
    }

    var off: usize = 0;
    var idx: usize = 0;
    while (idx < count) {
        const rem = count - idx;
        const line_length = std.math.min(@as(usize, max_line_length), rem);

        _ = out.write(padded_sequence[off .. off + line_length]) catch unreachable;
        _ = out.writeByte('\n') catch unreachable;

        off += line_length;
        if (off > sequence.len) {
            off -= sequence.len;
        }
        idx += line_length;
    }
}

fn generateAndWrap(out: anytype, comptime nucleotides: []const AminoAcid, count: usize) void {
    var cum_prob: f64 = 0;
    var cum_prob_total: [nucleotides.len]f64 = undefined;
    for (nucleotides, 0..) |n, i| {
        cum_prob += n.p;
        cum_prob_total[i] = cum_prob * im;
    }

    var line: [max_line_length + 1]u8 = undefined;
    line[max_line_length] = '\n';

    var idx: usize = 0;
    while (idx < count) {
        const rem = count - idx;
        const line_length = std.math.min(@as(usize, max_line_length), rem);

        for (line[0..line_length]) |*col| {
            const r = nextRandom(im);

            var c: usize = 0;
            for (cum_prob_total) |n| {
                if (n <= r) {
                    c += 1;
                }
            }

            col.* = nucleotides[c].l;
        }

        line[line_length] = '\n';
        _ = out.write(line[0 .. line_length + 1]) catch unreachable;
        idx += line_length;
    }
}

var buffer: [256]u8 = undefined;
var fixed_allocator = std.heap.FixedBufferAllocator.init(buffer[0..]);
var allocator = &fixed_allocator.allocator;

pub fn main() !void {
    var buffered_stdout = std.io.bufferedWriter(std.io.getStdOut().writer());
    defer buffered_stdout.flush() catch unreachable;
    const stdout = buffered_stdout.writer();
    const n = try get_n();
    const homo_sapiens_alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTC" ++
        "AGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCG" ++
        "TGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGG" ++
        "AGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
    _ = try stdout.write(">ONE Homo sapiens alu\n");
    repeatAndWrap(stdout, homo_sapiens_alu, 2 * n);

    const iub_nucleotide_info = &[_]AminoAcid{
        AminoAcid{ .l = 'a', .p = 0.27 },
        AminoAcid{ .l = 'c', .p = 0.12 },
        AminoAcid{ .l = 'g', .p = 0.12 },
        AminoAcid{ .l = 't', .p = 0.27 },
        AminoAcid{ .l = 'B', .p = 0.02 },
        AminoAcid{ .l = 'D', .p = 0.02 },
        AminoAcid{ .l = 'H', .p = 0.02 },
        AminoAcid{ .l = 'K', .p = 0.02 },
        AminoAcid{ .l = 'M', .p = 0.02 },
        AminoAcid{ .l = 'N', .p = 0.02 },
        AminoAcid{ .l = 'R', .p = 0.02 },
        AminoAcid{ .l = 'S', .p = 0.02 },
        AminoAcid{ .l = 'V', .p = 0.02 },
        AminoAcid{ .l = 'W', .p = 0.02 },
        AminoAcid{ .l = 'Y', .p = 0.02 },
    };
    _ = try stdout.write(">TWO IUB ambiguity codes\n");
    generateAndWrap(stdout, iub_nucleotide_info, 3 * n);

    const homo_sapien_nucleotide_info = &[_]AminoAcid{
        AminoAcid{ .l = 'a', .p = 0.3029549426680 },
        AminoAcid{ .l = 'c', .p = 0.1979883004921 },
        AminoAcid{ .l = 'g', .p = 0.1975473066391 },
        AminoAcid{ .l = 't', .p = 0.3015094502008 },
    };
    _ = try stdout.write(">THREE Homo sapiens frequency\n");
    generateAndWrap(stdout, homo_sapien_nucleotide_info, 5 * n);
}

fn get_n() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u64, arg, 10);
}
