const std = @import("std");
const expectEqualStrings = std.testing.expectEqualStrings;

const SortContext = struct {
    score_map: std.StringHashMap(u32),
};

fn ltFn(ctx: SortContext, lhs: []const u8, rhs: []const u8) bool {
    return ctx.score_map.get(lhs).? > ctx.score_map.get(rhs).?;
}

pub fn parseTrack(allocator: std.mem.Allocator, track_lines: []const u8) ![]const u8 {
    const newline_idx = std.mem.indexOf(u8, track_lines, "\n").?;
    const newline_count = std.mem.count(u8, track_lines, "\n");

    var track = try std.ArrayList(u8).initCapacity(allocator, 0);
    defer track.deinit(allocator);

    var x: usize = 0;
    var y: usize = 0;
    var dx: i8 = 1;
    var dy: i8 = 0;
    try track.append(allocator, track_lines[y * (newline_idx + 1) + x]);

    while (true) {
        if (x == newline_idx - 1 and dx == 1) {
            dx = 0;
            dy = 1;
        } else if (y == newline_count - 1 and dy == 1) {
            dx = -1;
            dy = 0;
        } else if (x == 0 and dx == -1) {
            dx = 0;
            dy = -1;
        }

        if (dx == -1) {
            x -= 1;
        }
        if (dx == 1) {
            x += 1;
        }
        if (dy == -1) {
            y -= 1;
        }
        if (dy == 1) {
            y += 1;
        }

        if (x == 0 and y == 0 and dx == 0 and dy == -1) {
            break;
        }

        try track.append(allocator, track_lines[y * (newline_idx + 1) + x]);
    }

    const result = try allocator.alloc(u8, track.items.len);
    @memcpy(result, track.items);
    return result;
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8, duration: u32) ![]const u8 {
    var s = std.StringHashMap(u32).init(allocator);
    var lines_it = std.mem.splitScalar(u8, lines, '\n');
    defer s.deinit();

    while (lines_it.next()) |line| {
        if (line.len == 0) {
            continue;
        }
        const sep = std.mem.indexOf(u8, line, ":").?;
        var score: u32 = 10;
        var total_power: u32 = 0;
        var i = sep + 1;
        var round: u32 = 0;

        while (round < duration) : (round += 1) {
            const ch = line[i];
            if (ch == '+') {
                score += 1;
            } else if (ch == '-') {
                score -= 1;
            } else if (ch == '#') {
                score += 0;
            }

            total_power += score;

            i += 2;
            if (i > line.len) {
                i = sep + 1;
            }
        }
        try s.put(line[0..sep], total_power);
    }

    var keys = try std.ArrayList([]const u8).initCapacity(allocator, s.count());
    defer keys.deinit(allocator);

    var key_it = s.keyIterator();
    while (key_it.next()) |p| {
        try keys.append(allocator, p.*);
    }

    std.mem.sort(
        []const u8,
        keys.items,
        SortContext{ .score_map = s },
        ltFn,
    );

    var result = try allocator.alloc(u8, keys.items.len);
    var j: u32 = 0;
    while (j < keys.items.len) : (j += 1) {
        result[j] = keys.items[j][0];
    }

    return result;
}

test "given example" {
    const lines = "A:+,-,=,=\nB:+,=,-,+\nC:=,-,+,+\nD:=,=,=,+\n";
    const allocator = std.testing.allocator;
    const result = try answer1(allocator, lines, 10);
    defer allocator.free(result);
    std.debug.print("{s}\n", .{result});

    try expectEqualStrings("BDCA", result);
}

test "track parsing" {
    const allocator = std.testing.allocator;

    const track_lines = "S+===\n-   +\n=+=-+\n";
    const result = try parseTrack(allocator, track_lines);
    defer allocator.free(result);

    try expectEqualStrings("S+===++-=+=-", result);
}
