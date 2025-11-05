const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;
const util = @import("util.zig");

const Wheel = struct {
    num_wheels: u8 = 0,
    increment: [4]u8 = [4]u8{ 0, 0, 0, 0 },
    position: [4]u8 = [4]u8{ 0, 0, 0, 0 },
    length: [4]u8 = [4]u8{ 0, 0, 0, 0 },
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
) [16]u8 {
    const trimmed_lines = lines[wheel.cycles_start..];
    var result: [16]u8 = std.mem.zeroes([16]u8);
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
    var counts: [7]u8 = std.mem.zeroes([7]u8);

    for (reading) |r| {
        if (r == ' ') {
            continue;
        }
        counts[
            switch (r) {
                '>' => 0,
                '<' => 1,
                '^' => 2,
                '_' => 3,
                '-' => 4,
                '.' => 5,
                ',' => 6,
                else => unreachable,
            }
        ] += 1;
    }

    var coins: u32 = 0;
    var i: u8 = 0;
    for (counts) |c| {
        if (!muzzles and i > 2) {
            break;
        }
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
}

pub fn answer1(lines: []const u8) ![16]u8 {
    // my input only has 4, so this is a way to avoid allocations
    // assumption: we have trailing whitespace (I've fixed my input)

    var wheel = try parseWheel(lines);

    for (0..100) |_| {
        // std.debug.print("{s}\n", .{wheelReading(lines, &wheel)});
        spinWheel(&wheel);
    }
    // std.debug.print("{s}\n", .{wheelReading(lines, &wheel)});

    return wheelReading(lines, &wheel);
}

test "given example" {
    const lines = "1,2,3\n\n^_^ -.- ^,-\n>.- ^_^ >.<\n-_- -.- >.<\n    -.^ ^_^\n    >.>    \n";
    try expectEqualStrings(">.- -.- ^,- ", (&try answer1(lines))[0..12]);
}
