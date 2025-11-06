const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const util = @import("util.zig");

const Wheel = struct {
    num_wheels: u8 = 0,
    increment: [10]u8 = std.mem.zeroes([10]u8),
    position: [10]u8 = std.mem.zeroes([10]u8),
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
) [40]u8 {
    const trimmed_lines = lines[wheel.cycles_start..];
    var result: [40]u8 = std.mem.zeroes([40]u8);
    var idx: usize = 0;
    for (0..wheel.num_wheels) |i| {
        const start = wheel.line_length * wheel.position[i] + (i * 4);
        @memcpy(result[idx .. idx + 3], trimmed_lines[start .. start + 3]);
        result[idx + 3] = ' ';
        idx += 4;
    }

    return result;
}

fn rightLever(wheel: *Wheel) void {
    for (0..wheel.num_wheels) |i| {
        wheel.position[i] = (wheel.position[i] + wheel.increment[i]) % wheel.length[i];
    }
}

fn leftLever(wheel: *Wheel, dpos: i8) void {
    for (0..wheel.num_wheels) |i| {
        if (dpos == -1) {
            if (wheel.position[i] == 0) {
                wheel.position[i] = wheel.length[i] - 1;
            } else {
                wheel.position[i] = (wheel.position[i] - 1) % wheel.length[i];
            }
        } else if (dpos == 1) {
            wheel.position[i] = (wheel.position[i] + 1) % wheel.length[i];
        }
    }
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

    try expectEqual(1, byteCoinsWon(">.- -.- ^_^", false));
}

pub fn answer1(lines: []const u8) ![40]u8 {
    // my input only has 4, so this is a way to avoid allocations
    // assumption: we have trailing whitespace (I've fixed my input)

    var wheel = try parseWheel(lines);

    for (0..100) |_| {
        rightLever(&wheel);
    }

    return wheelReading(lines, &wheel);
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8) !u64 {
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
            if (map.get(wheel.position)) |e| {
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
                try map.put(wheel.position, LoopEntry{ .coins_won = coins_won, .idx = i });
            }
        }

        rightLever(&wheel);
        const reading = wheelReading(lines, &wheel);
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

const QueueItem = struct {
    position: [10]u8,
    coins_so_far: u64,
    num_left: u64,
};

fn pQueueMax(_: void, a: QueueItem, b: QueueItem) std.math.Order {
    return std.math.order(a.coins_so_far, b.coins_so_far);
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8, num_pulls: u32) !?struct { u64, u64 } {
    // seems like this answer doesn't use muzzle = false
    var wheel = try parseWheel(lines);
    const starting_pos = wheel.position;

    var reverse_increment: [10]u8 = std.mem.zeroes([10]u8);
    for (0..wheel.num_wheels) |i| {
        reverse_increment[i] = wheel.length[i] - (wheel.increment[i] % wheel.length[i]);
    }
    var reverse_wheel = Wheel{ .cycles_start = wheel.cycles_start, .length = wheel.length, .line_length = wheel.line_length, .increment = reverse_increment, .num_wheels = wheel.num_wheels };

    // OK so we maintain a tree of states
    // then we go backwards.  Each state has a "max left from this position" / "min left from this position"
    // then the result is just min left from the start.

    const HistoryEntry = struct {
        position: [10]u8,
        pulls_left: u32,
    };
    const MinMaxEntry = struct {
        max: u64 = std.math.minInt(u64),
        min: u64 = std.math.maxInt(u64),
    };

    var map = std.AutoHashMap(HistoryEntry, MinMaxEntry).init(allocator);
    defer map.deinit();

    var states: std.ArrayList([10]u8) = try std.ArrayList([10]u8).initCapacity(allocator, 0);
    try states.append(allocator, std.mem.zeroes([10]u8));
    defer states.deinit(allocator);

    for (0..wheel.num_wheels) |i| {
        var next_states = try std.ArrayList([10]u8).initCapacity(allocator, states.items.len * wheel.length[i]);
        for (states.items) |p| {
            for (0..wheel.length[i]) |k| {
                var _p = p;
                _p[i] = @intCast(k);
                try next_states.append(allocator, _p);
            }
        }

        var _states = states;
        defer _states.deinit(allocator);
        states = next_states;
    }
    std.debug.print("state generation done \n", .{});

    // states now includes every state possible.  first we populate the $$$ per state as an initial.
    var i: u64 = 0;
    for (states.items) |state| {
        wheel.position = state;
        const reading = wheelReading(lines, &wheel);
        const coins = byteCoinsWon(&reading, true);
        try map.put(HistoryEntry{ .position = state, .pulls_left = num_pulls }, MinMaxEntry{ .max = coins, .min = coins });
        i += 1;
    }
    std.debug.print("map population done \n", .{});

    // so we go through every state.
    var pulls_left = num_pulls;
    while (pulls_left > 0) : (pulls_left -= 1) {
        std.debug.print("pull {d}\n", .{pulls_left});
        for (states.items) |state| {
            for ([3]i8{ 0, -1, 1 }) |dpos| {
                reverse_wheel.position = state;
                const prev = HistoryEntry{ .position = state, .pulls_left = pulls_left };
                const last_entry = map.get(prev).?;

                leftLever(&reverse_wheel, dpos);
                rightLever(&reverse_wheel);

                const reading = wheelReading(lines, &reverse_wheel);
                const coins = byteCoinsWon(&reading, true);

                const entry = HistoryEntry{ .position = reverse_wheel.position, .pulls_left = pulls_left - 1 };
                if (map.get(entry)) |e| {
                    var _e = e;
                    if (coins + last_entry.max > e.max) {
                        _e.max = coins + last_entry.max;
                    }
                    if (coins + last_entry.min < e.min) {
                        _e.min = coins + last_entry.min;
                    }
                    map.putAssumeCapacity(entry, _e);
                } else {
                    try map.put(entry, MinMaxEntry{ .max = coins + last_entry.max, .min = coins + last_entry.min });
                }
            }
        }
    }

    const first_entry = HistoryEntry{ .position = starting_pos, .pulls_left = 0 };
    if (map.get(first_entry)) |e| {
        wheel.position = starting_pos;

        // we've overcounted by the first one
        const reading = wheelReading(lines, &wheel);
        const coins = byteCoinsWon(&reading, true);

        std.debug.print("position coins = {d} | {d} {d}\n", .{ coins, e.max - coins, e.min - coins });
        return .{ e.max - coins, e.min - coins };
    }

    return null;
}

test "given example (part 3)" {
    const lines = "1,2,3\n\n^_^ -.- ^,-\n>.- ^_^ >.<\n-_- -.- ^.^\n    -.^ >.<\n    >.>    \n";
    try expectEqual(.{ 4, 2 }, try answer3(std.testing.allocator, lines, 1));
    try expectEqual(.{ 9, 2 }, try answer3(std.testing.allocator, lines, 256));
}
