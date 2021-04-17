const std = @import("std");
const bigint = std.math.big.int;
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var global_allocator = &gpa.allocator;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const n = try get_n();

    const one = (try bigint.Managed.initSet(global_allocator, 1)).toConst();
    const two = (try bigint.Managed.initSet(global_allocator, 2)).toConst();
    const ten = (try bigint.Managed.initSet(global_allocator, 10)).toConst();

    var k = try bigint.Managed.initSet(global_allocator, 1);
    var n1 = try bigint.Managed.initSet(global_allocator, 4);
    var n2 = try bigint.Managed.initSet(global_allocator, 3);
    var d = try bigint.Managed.initSet(global_allocator, 1);
    var u = try bigint.Managed.init(global_allocator);
    var v = try bigint.Managed.init(global_allocator);
    var w = try bigint.Managed.init(global_allocator);

    var digits_printed: usize = 0;
    var sb = [10:0]u8{ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' };
    while (true) {
        // u = &n1 / &d;
        {
            var tmp = try bigint.Managed.init(global_allocator);
            defer tmp.deinit();
            try bigint.Managed.divFloor(&u, &tmp, n1.toConst(), d.toConst());
        }
        // v = &n2 / &d;
        {
            var tmp = try bigint.Managed.init(global_allocator);
            defer tmp.deinit();
            try bigint.Managed.divFloor(&v, &tmp, n2.toConst(), d.toConst());
        }
        // if u == v
        if (bigint.Managed.eq(u, v)) {
            var digitStr = try u.toString(global_allocator, 10, false);
            const rem = @rem(digits_printed, 10);
            sb[rem] = digitStr[0];
            digits_printed += 1;
            if (rem == 9) {
                try stdout.print("{}\t:{}\n", .{ sb, digits_printed });
            }

            if (digits_printed >= n) {
                if (rem != 9) {
                    var i = rem + 1;
                    while (i < 10) {
                        sb[i] = ' ';
                        i += 1;
                    }
                    try stdout.print("{}\t:{}\n", .{ sb, digits_printed });
                }
                break;
            }

            // let to_minus = &u * &ten * &d;
            var to_minus_managed = try bigint.Managed.init(global_allocator);
            defer to_minus_managed.deinit();
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.mul(&tmp, u.toConst(), d.toConst());
                try bigint.Managed.mul(&to_minus_managed, tmp.toConst(), ten);
            }
            const to_minus = to_minus_managed.toConst();
            // n1 = &n1 * &ten - &to_minus;
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.mul(&tmp, n1.toConst(), ten);
                try bigint.Managed.sub(&n1, tmp.toConst(), to_minus);
            }
            // n2 = &n2 * &ten - &to_minus;
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.mul(&tmp, n2.toConst(), ten);
                try bigint.Managed.sub(&n2, tmp.toConst(), to_minus);
            }
        } else {
            // let k2 = &k * &two;
            var k2 = try bigint.Managed.init(global_allocator);
            defer k2.deinit();
            try bigint.Managed.mul(&k2, k.toConst(), two);
            // u = &n1 * (&k2 - &one);
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.sub(&tmp, k2.toConst(), one);
                var tmpu = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpu, tmp.toConst(), n1.toConst());
                u.deinit();
                u = tmpu;
            }
            // v = &n2 * &two;
            {
                var tmpv = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpv, n2.toConst(), two);
                v.deinit();
                v = tmpv;
            }
            // w = &n1 * (&k - &one);
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.sub(&tmp, k.toConst(), one);
                var tmpw = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpw, tmp.toConst(), n1.toConst());
                w.deinit();
                w = tmpw;
            }
            // n1 = &u + &v;
            try bigint.Managed.add(&n1, u.toConst(), v.toConst());
            // u = &n2 * (&k + &two);
            {
                var tmp = try bigint.Managed.init(global_allocator);
                defer tmp.deinit();
                try bigint.Managed.add(&tmp, k.toConst(), two);
                var tmpu = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpu, tmp.toConst(), n2.toConst());
                u.deinit();
                u = tmpu;
            }
            // n2 = &w + &u;
            try bigint.Managed.add(&n2, w.toConst(), u.toConst());
            // d = &d * (&k2 + &one);
            {
                var tmp1 = try bigint.Managed.init(global_allocator);
                defer tmp1.deinit();
                try bigint.Managed.add(&tmp1, k2.toConst(), one);
                var tmpd = try bigint.Managed.init(global_allocator);
                try bigint.Managed.mul(&tmpd, tmp1.toConst(), d.toConst());
                d.deinit();
                d = tmpd;
            }

            // k = &k + &one;
            try bigint.Managed.add(&k, k.toConst(), one);
        }
    }
}

fn get_n() !i32 {
    const args = try std.process.argsAlloc(global_allocator);
    defer std.process.argsFree(global_allocator, args);
    if (args.len > 1) {
        return try std.fmt.parseInt(i32, args[1], 10);
    } else {
        return 1;
    }
}
