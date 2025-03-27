const std = @import("std");
const math = std.math;

const solar_mass = 4.0 * math.pi * math.pi;
const year = 365.24;

const vec3 = @Vector(3, f64);

fn scale(v: anytype, f: f64) @TypeOf(v) {
    return v * @as(@TypeOf(v), @splat(f));
}

fn lengthSq(v: vec3) f64 {
    return @reduce(.Add, v * v);
}

fn length(v: vec3) f64 {
    return @sqrt(lengthSq(v));
}

const Body = struct {
    pos: vec3,
    vel: vec3,
    mass: f64,
};

fn offsetMomentum(bodies: []Body) void {
    @setFloatMode(.optimized);
    var pos: vec3 = @splat(0);
    for (bodies[1..]) |b| pos += scale(@as(vec3, b.vel), b.mass);
    bodies[0].vel = -scale(pos, 1.0 / solar_mass);
}

fn allPairs(comptime n: usize) [n * (n - 1)/2][2]u32 {
    var res: [n * (n - 1)/2][2]u32 = undefined;
    var k: usize = 0;
    for (0..n - 1) |i| for (i + 1..n) |j| {
        res[k] = .{@intCast(i), @intCast(j)};
        k += 1;
    };
    return res;
}

fn advance(comptime n: usize, bodies: *[n]Body, dt: f64) void {
    @setFloatMode(.optimized);
    const pairs = comptime allPairs(n);
    var dp: [pairs.len]vec3 = undefined;
    var distSq: @Vector(pairs.len, f64) = undefined;
    inline for (pairs, 0..) |p, i| {
        const d = bodies[p[0]].pos - bodies[p[1]].pos;
        dp[i] = d;
        distSq[i] = lengthSq(dp[i]);
    }
    const mag = @as(@Vector(pairs.len, f64), @splat(dt)) / (distSq * @sqrt(distSq));

    inline for (pairs, 0..) |p, i| {
        bodies[p[0]].vel -= scale(dp[i], bodies[p[1]].mass * mag[i]);
        bodies[p[1]].vel += scale(dp[i], bodies[p[0]].mass * mag[i]);
    }

    inline for (bodies) |*body| body.pos += scale(body.vel, dt);
}

fn energy(bodies: []const Body) f64 {
    @setFloatMode(.optimized);
    var e: f64 = 0.0;
    for (bodies, 0..) |bi, i| {
        e += 0.5 * lengthSq(bi.vel) * bi.mass;
        for (bodies[i + 1 ..]) |bj| {
            e -= bi.mass * bj.mass / length(bi.pos - bj.pos);
        }
    }
    return e;
}

var solar_bodies = [_]Body{
    // Sun
    Body{
        .pos = @splat(0),
        .vel = @splat(0),
        .mass = solar_mass,
    },
    // Jupiter
    Body{
        .pos = .{ 4.84143144246472090, -1.16032004402742839, -0.103622044471123109 },
        .vel = scale(vec3{ 1.66007664274403694e-03, 7.69901118419740425e-03, -6.90460016972063023e-05 }, year),
        .mass = 9.54791938424326609e-04 * solar_mass,
    },
    // Saturn
    Body{
        .pos = .{ 8.34336671824457987, 4.12479856412430479, -0.403523417114321381 },
        .vel = scale(vec3{ -2.76742510726862411e-03, 4.99852801234917238e-03, 2.30417297573763929e-05 }, year),
        .mass = 2.85885980666130812e-04 * solar_mass,
    },
    // Uranus
    Body{
        .pos = .{ 12.8943695621391310, -15.1111514016986312, -0.223307578892655734 },
        .vel = scale(vec3{ 2.96460137564761618e-03, 2.37847173959480950e-03, -2.96589568540237556e-05 }, year),
        .mass = 4.36624404335156298e-05 * solar_mass,
    },
    // Neptune
    Body{
        .pos = .{ 15.3796971148509165, -25.9193146099879641, 0.179258772950371181 },
        .vel = scale(vec3{ 2.68067772490389322e-03, 1.62824170038242295e-03, -9.51592254519715870e-05 }, year),
        .mass = 5.15138902046611451e-05 * solar_mass,
    },
};

pub fn main() !void {
    const steps = try getSteps();

    offsetMomentum(&solar_bodies);
    const initial_energy = energy(&solar_bodies);
    for (0..steps) |_| advance(solar_bodies.len, &solar_bodies, 0.01);
    const final_energy = energy(&solar_bodies);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d:.9}\n{d:.9}\n", .{ initial_energy, final_energy });
}

fn getSteps() !usize {
    var arg_it = std.process.args();
    _ = arg_it.skip();
    const arg = arg_it.next() orelse return 1000;
    return try std.fmt.parseInt(usize, arg, 10);
}
