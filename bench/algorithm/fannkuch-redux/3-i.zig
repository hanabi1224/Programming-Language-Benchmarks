// port of 3-i.rs
// uses simd: requires x86_64 and sse2
// uses inline assembly to implement _mm_shuffle_epi8 as zig doesn't yet have this builtin.
// zig does have a `@shuffle` builtin which requires a comptime known mask argument.  using this
// requires a large switch or inline while which is much slower than using vpshufb directly
//   (as shown in 2.zig: https://github.com/hanabi1224/Programming-Language-Benchmarks/blob/main/bench/algorithm/fannkuch-redux/2.zig#L30)
//
//
// another way to accomplish this would be to provide a small c wrapper with the function:
//
//  // shuffle.c
//  #include <tmmintrin.h>
//  __m128i shuffle_epi8 (__m128i a, __m128i b) {
//    return _mm_shuffle_epi8(a, b);
//  }
//
// then use the following extern rather than the inline assembly version,
// adding 'shuffle.c' on the command line. (zig is also a c/c++ compiler)
//
//  const c = @cImport(@cInclude("tmmintrin.h"));
//  extern fn shuffle_epi8 (c.__m128i, c.__m128i) c__m128i;
//
//

const std = @import("std");
const u8x16 = @Vector(16, u8);

fn get_n() !u4 {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 10;
    return try std.fmt.parseInt(u4, arg, 10);
}

pub fn main() !void {
    const n = try get_n();
    const x = fannkuchRedux(n);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\nPfannkuchen({}) = {}\n", .{ x[0], n, x[1] });
}

inline fn shuffle_epi8(x: u8x16, mask: u8x16) u8x16 {
    // make sure arch is x86_64 and we have sse2 feature set for vpshufb
    const builtin = @import("builtin");
    const has_sse2 = comptime std.Target.x86.featureSetHas(builtin.cpu.features, .sse2);
    if (builtin.cpu.arch != .x86_64 or !has_sse2)
        @compileError("missing cpu feature set: x86_64+sse2. please provide -mcpu=x86_64+sse2");

    // __asm__("pshufb %1, %0" : "+x" (mmdesc) : "xm" (shuf_mask));
    const shuffled = asm (
        \\ vpshufb %[mask], %[x], %[out]
        : [out] "=x" (-> u8x16),
        : [x] "+x" (x),
          [mask] "x" (mask),
    );
    return shuffled;
}

inline fn reverse_array(array: u128, n: u4) u128 {
    const MASK = [16]u128{
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01_00,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_00_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_00_01_02,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_00_01_02_03,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_00_01_02_03_04,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_00_01_02_03_04_05,
        0x0F_0E_0D_0C_0B_0A_09_08_07_00_01_02_03_04_05_06,
        0x0F_0E_0D_0C_0B_0A_09_08_00_01_02_03_04_05_06_07,
        0x0F_0E_0D_0C_0B_0A_09_00_01_02_03_04_05_06_07_08,
        0x0F_0E_0D_0C_0B_0A_00_01_02_03_04_05_06_07_08_09,
        0x0F_0E_0D_0C_0B_00_01_02_03_04_05_06_07_08_09_0A,
        0x0F_0E_0D_0C_00_01_02_03_04_05_06_07_08_09_0A_0B,
        0x0F_0E_0D_00_01_02_03_04_05_06_07_08_09_0A_0B_0C,
        0x0F_0E_00_01_02_03_04_05_06_07_08_09_0A_0B_0C_0D,
        0x0F_00_01_02_03_04_05_06_07_08_09_0A_0B_0C_0D_0E,
        0x00_01_02_03_04_05_06_07_08_09_0A_0B_0C_0D_0E_0F,
    };
    return @bitCast(u128, shuffle_epi8(@bitCast(u8x16, array), @bitCast(u8x16, MASK[n])));
}

inline fn rotate_array(array: u128, n: u4) u128 {
    const MASK = [16]u128{
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01_00,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_00_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_00_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_00_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_00_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_06_00_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_07_00_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_08_00_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_09_00_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_0A_00_09_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_0B_00_0A_09_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_0C_00_0B_0A_09_08_07_06_05_04_03_02_01,
        0x0F_0E_0D_00_0C_0B_0A_09_08_07_06_05_04_03_02_01,
        0x0F_0E_00_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01,
        0x0F_00_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01,
        0x00_0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01,
    };
    return @bitCast(u128, shuffle_epi8(@bitCast(u8x16, array), @bitCast(u8x16, MASK[n])));
}

inline fn advance_array(_array: u128, count: *[16]u8) u128 {
    var array = _array;
    var layer: u4 = 1;
    while (layer < 16) : (layer += 1) {
        array = rotate_array(array, layer);
        count[layer] += 1;
        if (count[layer] <= layer) break;
        count[layer] = 0;
    }
    return array;
}

fn fannkuchRedux(n: u4) [2]i32 {
    var _current: u128 = 0x0F_0E_0D_0C_0B_0A_09_08_07_06_05_04_03_02_01_00;
    var _count = [1]u8{0} ** 16;
    var max_rev: i32 = 0;

    // Trivial cases, not implemented
    if (n == 0) return [2]i32{ 0, 0 };
    if (n == 1) return [2]i32{ 0, 0 };
    if (n == 2) return [2]i32{ -1, 1 };

    var arrays = [1]u128{0} ** 16;
    arrays[0] = _current;
    {
        var i: u4 = 1;
        while (i < n) : (i += 1) {
            _current = rotate_array(_current, n - 1);
            arrays[i] = _current;
        }
    }
    var rotate_count: u4 = 0;
    var checksum: i32 = 0;
    while (rotate_count < n) : (rotate_count += 1) {
        var current = arrays[rotate_count];
        var count = _count;
        count[n - 1] = rotate_count;

        // Calculating checksum and max_rev
        var arrays2 = [1]u128{0} ** 16;
        arrays2[0] = current;
        {
            var i: u4 = 1;
            while (i < n - 1) : (i += 1) {
                current = rotate_array(current, n - 2);
                arrays2[i] = current;
            }
        }
        var rotate_count2: u4 = 0;
        while (rotate_count2 < n - 1) : (rotate_count2 += 1) {
            var current2 = arrays2[rotate_count2];
            var count2 = count;
            count2[n - 2] = rotate_count2;
            while (count2[n - 2] == rotate_count2) {
                var tmp = current2;
                var rev_count: i32 = 0;
                var first = @bitCast(u8x16, tmp)[0];
                if (first > 0) {
                    while (first > 0) {
                        const next = @bitCast(u8x16, tmp)[first];
                        tmp = reverse_array(tmp, @truncate(u4, first));
                        first = next;
                        rev_count += 1;
                    }
                    // Bit hack: conditional negation, oddly impactful on performance
                    checksum += (rev_count ^ -@as(i32, count2[1])) + count2[1];
                    if (rev_count > max_rev) max_rev = rev_count;
                }
                current2 = advance_array(current2, &count2);
            }
        }
    }

    return [2]i32{ checksum, max_rev };
}
