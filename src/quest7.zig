const std = @import("std");
const util = @import("util.zig");
const Direction = util.Direction;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const SortContext = struct {
    score_map: std.StringHashMap(u32),
};

fn ltFn(ctx: SortContext, lhs: []const u8, rhs: []const u8) bool {
    return ctx.score_map.get(lhs).? > ctx.score_map.get(rhs).?;
}

pub fn parseTrack(allocator: std.mem.Allocator, track_lines: []const u8) ![]const u8 {
    const grid = util.createGrid(track_lines);

    var track = try std.ArrayList(u8).initCapacity(allocator, 0);
    defer track.deinit(allocator);

    var direction: Direction = Direction.right;
    var idx: usize = 1;

    while (true) {
        try track.append(allocator, track_lines[idx]);
        const next = util.walk(grid, idx, direction, .{});
        if (next == null or track_lines[next.?] == ' ') {
            if (direction == Direction.right or direction == Direction.left) {
                const next_up = util.walk(grid, idx, Direction.up, .{});
                const next_down = util.walk(grid, idx, Direction.down, .{});

                if (next_up != null and track_lines[next_up.?] != ' ') {
                    direction = Direction.up;
                } else if (next_down != null and track_lines[next_down.?] != ' ') {
                    direction = Direction.down;
                }
            } else if (direction == Direction.up or direction == Direction.down) {
                const next_left = util.walk(grid, idx, Direction.left, .{});
                const next_right = util.walk(grid, idx, Direction.right, .{});

                if (next_left != null and track_lines[next_left.?] != ' ') {
                    direction = Direction.left;
                } else if (next_right != null and track_lines[next_right.?] != ' ') {
                    direction = Direction.right;
                }
            }
        }
        idx = util.walk(grid, idx, direction, .{}).?;

        if (idx == 0) {
            try track.append(allocator, track_lines[idx]);
            break;
        }
    }

    const result = try allocator.alloc(u8, track.items.len);
    @memcpy(result, track.items);
    return result;
}

pub fn answer(allocator: std.mem.Allocator, lines: []const u8, track: []const u8, numLaps: u32) ![]const u8 {
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
        var track_position: usize = 0;

        while (round < numLaps) {
            const track_ch = track[track_position];
            if (track_ch == '+') {
                score += 1;
            } else if (track_ch == '-') {
                score -= 1;
            } else {
                const ch = line[i];
                if (ch == '+') {
                    score += 1;
                } else if (ch == '-') {
                    score -= 1;
                } else if (ch == '#') {
                    score += 0;
                }
            }

            total_power += score;
            track_position = (track_position + 1) % track.len;
            if (track_ch == 'S') {
                round += 1;
            }

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
        // std.debug.print("{s} - {d}\n", .{ p.*, s.get(p.*).? });
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

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8, numLaps: u32) ![]const u8 {
    return answer(allocator, lines, "S", numLaps);
}

pub fn answer2(allocator: std.mem.Allocator, lines: []const u8, numLaps: u32) ![]const u8 {
    const track_lines = "S-=++=-==++=++=-=+=-=+=+=--=-=++=-==++=-+=-=+=-=+=+=++=-+==++=++=-=-=--\n-                                                                     -\n=                                                                     =\n+                                                                     +\n=                                                                     +\n+                                                                     =\n=                                                                     =\n-                                                                     -\n--==++++==+=+++-=+=-=+=-+-=+-=+-=+=-=+=--=+++=++=+++==++==--=+=++==+++-\n";
    const track = try parseTrack(allocator, track_lines);
    defer allocator.free(track);

    return answer(allocator, lines, track, numLaps);
}

test "given example" {
    const lines = "A:+,-,=,=\nB:+,=,-,+\nC:=,-,+,+\nD:=,=,=,+\n";
    const allocator = std.testing.allocator;
    const result = try answer(allocator, lines, "S", 10);
    defer allocator.free(result);
    std.debug.print("{s}\n", .{result});

    try expectEqualStrings("BDCA", result);
}

test "track parsing" {
    const allocator = std.testing.allocator;

    const track_lines = "S+===\n-   +\n=+=-+\n";
    const result = try parseTrack(allocator, track_lines);
    defer allocator.free(result);

    try expectEqualStrings("+===++-=+=-S", result);
}

test "given example (part 2)" {
    const lines = "A:+,-,=,=\nB:+,=,-,+\nC:=,-,+,+\nD:=,=,=,+\n";
    const allocator = std.testing.allocator;

    const track_lines = "S+===\n-   +\n=+=-+\n";
    const track = try parseTrack(allocator, track_lines);
    defer allocator.free(track);

    const result = try answer(allocator, lines, track, 10);
    defer allocator.free(result);

    try expectEqualStrings("DCBA", result);
}

test "more difficult track parsing" {
    const track_lines = "S+=  =-=\n- =+-+ +\n+      +\n=+=-+--=\n";
    const allocator = std.testing.allocator;

    const result = try parseTrack(allocator, track_lines);
    defer allocator.free(result);

    try expectEqualStrings("+==+-+=-=++=--+-=+=+-S", result);
}

// 462 * 20 = 9240 possibilities of valid action plans
// brute forceable for sure
// writing brute force logic sure is annoying
// we'll use a lexicographical permutation generator

// https://stemhash.com/efficient-permutations-in-lexicographic-order/
// step 1) Find the largest index i such that s[i]<s[i+1]. If we can't find such an index, it means we are at the last permutation of the sequence:
//     From our list, i=2i=2, because the number at index 2, s[2]=3s[2]=3 is less than the number at index i+1=3i+1=3, s[3]=4s[3]=4, and i=2i=2 is the largest index that satisfies this condition.

// step 2) Find the largest index j that is greater than i, such that s[i]<s[j]:
//     Looking at our list we get j=3j=3. This satisfies our condition (s[2]=3)<(s[3]=4)(s[2]=3)<(s[3]=4)

// step 3) Swap the value of s[i] with that of s[j]]:
//     So, we swap the values at the two indices. Our list becomes: s=[1,2,4,3]s=[1,2,4,3]

// step 4) Reverse the sequence from s[i+1] up to and including the last element:
//     In our case, i=2. So, the next index i+1=3i+1=3, which is the last index of our list, therefore it stays the same. Our next permutation is s=[1,2,4,3]s=[1,2,4,3].

// We'll say - < = < +

fn cmpOperator(ch1: u8, ch2: u8) i8 {
    if (ch1 == ch2) {
        return 0;
    }

    if (ch1 == '-') {
        if (ch2 == '+' or ch2 == '=') {
            return -1;
        }
        unreachable;
    }

    if (ch1 == '=') {
        if (ch2 == '+') {
            return -1;
        }
        if (ch2 == '-') {
            return 1;
        }
        unreachable;
    }

    if (ch1 == '+') {
        if (ch2 == '=' or ch2 == '-') {
            return 1;
        }
        unreachable;
    }

    unreachable;
}

test "cmpOperator" {
    try expectEqual(-1, cmpOperator('-', '='));
    try expectEqual(-1, cmpOperator('-', '+'));
    try expectEqual(0, cmpOperator('-', '-'));

    try expectEqual(0, cmpOperator('=', '='));
    try expectEqual(-1, cmpOperator('=', '+'));
    try expectEqual(1, cmpOperator('=', '-'));

    try expectEqual(1, cmpOperator('+', '='));
    try expectEqual(0, cmpOperator('+', '+'));
    try expectEqual(1, cmpOperator('+', '-'));
}
