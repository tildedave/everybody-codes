const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const util = @import("util.zig");

const Wheel = struct {
    num_wheels: u8 = 0,
    increment: [10]u8 = std.mem.zeroes([10]u8),
    length: [10]u8 = std.mem.zeroes([10]u8),
    line_length: usize = 0,
    cycles_start: usize = 0,
};

fn parseWheel(lines: []const u8) !Wheel {
    // my input only has 4, so this is a way to avoid allocations
    // assumption: we have trailing whitespace (I've fixed my input)
    var wheel = Wheel{};

    const first_line_end = std.mem.indexOfScalar(u8, lines, '\n').?;

    var it = std.mem.splitScalar(u8, lines[0..first_line_end], ',');
    while (it.next()) |s| {
        wheel.increment[wheel.num_wheels] = try std.fmt.parseInt(u8, s, 10);
        wheel.num_wheels += 1;
    }

    wheel.cycles_start = first_line_end + 2;
    var line_it = std.mem.splitScalar(u8, lines[wheel.cycles_start..], '\n');
    while (line_it.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        wheel.line_length = line.len + 1;
        for (0..wheel.num_wheels) |i| {
            if (line[i * 4] != ' ') {
                wheel.length[i] += 1;
            }
        }
    }

    return wheel;
}

fn wheelReading(
    lines: []const u8,
    wheel: *const Wheel,
    position: *const [10]u8,
) [40]u8 {
    const trimmed_lines = lines[wheel.cycles_start..];
    var result: [40]u8 = std.mem.zeroes([40]u8);
    var idx: usize = 0;
    for (0..wheel.num_wheels) |i| {
        const start = wheel.line_length * position[i] + (i * 4);
        @memcpy(result[idx .. idx + 3], trimmed_lines[start .. start + 3]);
        result[idx + 3] = ' ';
        idx += 4;
    }

    return result;
}

fn rightLever(wheel: *const Wheel, position: *const [10]u8) [10]u8 {
    var next_position = std.mem.zeroes([10]u8);
    for (0..wheel.num_wheels) |i| {
        next_position[i] = (position[i] + wheel.increment[i]) % wheel.length[i];
    }

    return next_position;
}

fn leftLever(wheel: *const Wheel, position: *const [10]u8, dpos: i8) [10]u8 {
    var next_position = std.mem.zeroes([10]u8);

    for (0..wheel.num_wheels) |i| {
        if (dpos == -1) {
            if (position[i] == 0) {
                next_position[i] = wheel.length[i] - 1;
            } else {
                next_position[i] = (position[i] - 1) % wheel.length[i];
            }
        } else if (dpos == 1) {
            next_position[i] = (position[i] + 1) % wheel.length[i];
        } else {
            next_position[i] = position[i];
        }
    }

    return next_position;
}

fn byteCoinsWon(reading: []const u8, muzzles: bool) u32 {
    var counts: [255]u8 = std.mem.zeroes([255]u8);

    var min_c: u8 = std.math.maxInt(u8);
    var max_c: u8 = std.math.minInt(u8);
    var i: u8 = 0;
    // std.debug.print("bazinga {s} {any}\n", .{ reading, counts });
    for (reading) |r| {
        if (r != ' ' and r != 0) {
            if (!muzzles and (i % 2) != 0) {} else {
                // std.debug.print("{c} {d} {d}\n", .{ r, r, counts[r] + 1 });
                counts[r] += 1;
            }
        }

        i += 1;
        min_c = @min(min_c, r);
        max_c = @max(max_c, r);
    }

    var coins: u32 = 0;
    i = 0;
    for (counts[min_c .. max_c + 1]) |c| {
        if (c >= 3) {
            coins += c - 2;
        }
        i += 1;
    }

    return coins;
}

test "byteCoinsWon" {
    try expectEqual(1, byteCoinsWon(">.- -.- ^_^", true));
    try expectEqual(1, byteCoinsWon("-_- >.> >.<", true));
    try expectEqual(2, byteCoinsWon("^_^ ^_^ >.<", true));
    try expectEqual(1, byteCoinsWon(">.- -.^ ^,-", true));
    try expectEqual(2, byteCoinsWon("-_- -.- ^_^", true));
    try expectEqual(2, byteCoinsWon("^_^ -.- ^_^", true));
    try expectEqual(5, byteCoinsWon("^_^ ^_^ ^_^", true));

    try expectEqual(1, byteCoinsWon(">.- -.- >.<", false));
    try expectEqual(1, byteCoinsWon(">.- -.- ^_^", false));
    try expectEqual(2, byteCoinsWon("-_- -.^ ^,-", false));
    try expectEqual(4, byteCoinsWon("^_^ ^_^ ^_^", false));
}

pub fn answer1(lines: []const u8) ![40]u8 {
    // my input only has 4, so this is a way to avoid allocations
    // assumption: we have trailing whitespace (I've fixed my input)

    var position = std.mem.zeroes([10]u8);
    var wheel = try parseWheel(lines);

    for (0..100) |_| {
        position = rightLever(&wheel, &position);
    }

    return wheelReading(lines, &wheel, &position);
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    var position = std.mem.zeroes([10]u8);
    var wheel = try parseWheel(lines);

    const LoopEntry = struct {
        idx: u64,
        coins_won: u64,
    };

    var map = std.AutoHashMap([10]u8, LoopEntry).init(allocator);
    defer map.deinit();

    var coins_won: u64 = 0;
    var i: u64 = 0;
    const num_loops = 202420242024;
    var found_cycle = false;
    while (i < num_loops) : (i += 1) {
        // std.debug.print("{d}: {d}\n", .{ i, coins_won });

        if (!found_cycle) {
            if (map.get(position)) |e| {
                const coin_delta = coins_won - e.coins_won;
                const idx_delta = i - e.idx;
                // std.debug.print("loop after {d} iterations, previous was {d}, {d} net coins ({d} vs {d})\n", .{ i, e.idx, coin_delta, coins_won, e.coins_won });

                const loops_left = num_loops - i;
                const num_times = loops_left / idx_delta;
                // std.debug.print("repeat {d} times\n", .{num_times});

                i += idx_delta * num_times;
                coins_won += coin_delta * num_times;
                found_cycle = true;
                // std.debug.print("{d}: {d}\n", .{ i, coins_won });
            } else {
                try map.put(position, LoopEntry{ .coins_won = coins_won, .idx = i });
            }
        }

        position = rightLever(&wheel, &position);
        const reading = wheelReading(lines, &wheel, &position);
        const coins = byteCoinsWon(&reading, false);
        coins_won += coins;
    }

    return coins_won;
}

test "given example (part 1)" {
    const lines = "1,2,3\n\n^_^ -.- ^,-\n>.- ^_^ >.<\n-_- -.- >.<\n    -.^ ^_^\n    >.>    \n";
    try expectEqualStrings(">.- -.- ^,- ", (&try answer1(lines))[0..12]);
}

test "given example (part 2)" {
    const lines = "1,2,3\n\n^_^ -.- ^,-\n>.- ^_^ >.<\n-_- -.- >.<\n    -.^ ^_^\n    >.>    \n";
    try expectEqual(280014668134, try answer2(std.testing.allocator, lines));
}

// part 3 is sort of a graph algo.  total space of possible spaces is small (length1 * length2 * ...)
// problem is that the number of paths through it are too large (3^256).  can't try every path.
// feels like some aggressive pruning will be fine.  and we can run through the state trying to optimize and minimize (two runs).
// sadly must do a bunch of copying

const State = struct {
    position: [10]u8,
    pulls_left: u32,
};
const MinMaxResult = struct {
    max: u64 = std.math.minInt(u64),
    min: u64 = std.math.maxInt(u64),
};

// dynamic programming
fn computeState(lines: []const u8, wheel: *Wheel, map: *std.AutoHashMap(State, MinMaxResult), position: [10]u8, pulls_left: u32) !MinMaxResult {
    const state = State{ .position = position, .pulls_left = pulls_left };
    if (map.get(state)) |r| {
        return r;
    }

    // const reading = wheelReading(lines, wheel, &position);
    // std.debug.print("{d}: {s}\n", .{ pulls_left, reading });
    if (pulls_left == 0) {
        const r = MinMaxResult{ .max = 0, .min = 0 };
        try map.put(state, r);

        return r;
    }

    var min_coins: u64 = std.math.maxInt(u64);
    var max_coins: u64 = std.math.minInt(u64);

    for ([3]i8{ 0, 1, -1 }) |dpos| {
        const left_position = leftLever(wheel, &position, dpos);
        const next_position = rightLever(wheel, &left_position);

        // const _left_reading = wheelReading(lines, wheel, &left_position);
        const _reading = wheelReading(lines, wheel, &next_position);
        const coins = byteCoinsWon(&_reading, false);
        const r = try computeState(lines, wheel, map, next_position, pulls_left - 1);
        // std.debug.print("{d}: {s} ({any}) {s} ({any}) {s} ({any}) (coins: {d}, max: {d}, min: {d})\n", .{ pulls_left, reading, position, _left_reading, left_position, _reading, next_position, coins, r.max + coins, r.min + coins });

        max_coins = @max(r.max + coins, max_coins);
        min_coins = @min(r.min + coins, min_coins);
    }
    const r = MinMaxResult{ .max = max_coins, .min = min_coins };
    try map.put(state, r);

    // std.debug.print("{d}: {s} ({d} max, {d} min)\n", .{ pulls_left, reading, r.max, r.min });

    return r;
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8, num_pulls: u32) !?struct { u64, u64 } {
    // seems like this answer doesn't use muzzle = false j/k
    var wheel = try parseWheel(lines);

    // OK so we maintain a tree of states
    // then we go backwards.  Each state has a "max left from this position" / "min left from this position"
    // then the result is just min left from the start.

    var map = std.AutoHashMap(State, MinMaxResult).init(allocator);
    defer map.deinit();

    const r = try computeState(lines, &wheel, &map, std.mem.zeroes([10]u8), num_pulls);
    return .{ r.max, r.min };
}

test "given example (part 3)" {
    const lines = "1,2,3\n\n^_^ -.- ^,-\n>.- ^_^ >.<\n-_- -.- ^.^\n    -.^ >.<\n    >.>    \n";
    try expectEqual(.{ 4, 1 }, try answer3(std.testing.allocator, lines, 1));
    try expectEqual(.{ 6, 1 }, try answer3(std.testing.allocator, lines, 2));
    try expectEqual(.{ 9, 2 }, try answer3(std.testing.allocator, lines, 3));
    try expectEqual(.{ 26, 5 }, try answer3(std.testing.allocator, lines, 10));
    try expectEqual(.{ 627, 128 }, try answer3(std.testing.allocator, lines, 256));
    try expectEqual(.{ 4948, 1012 }, try answer3(std.testing.allocator, lines, 2024));
}
