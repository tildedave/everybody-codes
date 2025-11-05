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

fn spinWheel(wheel: *Wheel) void {
    for (0..wheel.num_wheels) |i| {
        wheel.position[i] = (wheel.position[i] + wheel.increment[i]) % wheel.length[i];
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
        spinWheel(&wheel);
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

        spinWheel(&wheel);
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
