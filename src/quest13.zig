const std = @import("std");
const expectEqual = std.testing.expectEqual;
const util = @import("util.zig");

const SearchError = error{Unreachable};

fn level(ch: u8) u8 {
    return switch (ch) {
        'S' => 0,
        'E' => 0,
        else => ch - '0',
    };
}

fn distance(ch1: u8, ch2: u8) u8 {
    const l1 = level(ch1);
    const l2 = level(ch2);

    if (l1 == l2) {
        return 1;
    }
    if (l1 > l2) {
        if (l1 - l2 > 5) {
            return 10 + l2 - l1 + 1;
        }
        return l1 - l2 + 1;
    }
    if (l2 - l1 > 5) {
        return 10 + l1 - l2 + 1;
    }
    return l2 - l1 + 1;
}

test "level" {
    try expectEqual(0, level('S'));
    try expectEqual(0, level('0'));
    try expectEqual(1, level('1'));
    try expectEqual(2, level('2'));
    try expectEqual(3, level('3'));
    try expectEqual(4, level('4'));
    try expectEqual(5, level('5'));
    try expectEqual(9, level('9'));
}
test "distance" {
    try expectEqual(6, distance('S', '5'));
    try expectEqual(2, distance('9', '0'));
    try expectEqual(3, distance('9', '1'));
    try expectEqual(4, distance('9', '2'));
    try expectEqual(5, distance('9', '3'));
    try expectEqual(6, distance('9', '4'));
    try expectEqual(5, distance('9', '5'));
}

fn isEndAnswer1(ch: u8) bool {
    return ch == 'E';
}

const SearchContext = struct {
    grid: util.Grid,
};

pub fn gridNeighbors(node: usize, allocator: std.mem.Allocator, l: *std.ArrayList(usize), context: SearchContext) !void {
    var it = util.NeighborIterator{ .grid = context.grid, .idx = node };
    while (it.next()) |ni| {
        if (ni == '#' or ni == ' ') {
            continue;
        }

        try l.append(allocator, it.next_idx);
    }
}

pub fn gridDistance(n1: usize, n2: usize, context: SearchContext) u32 {
    return distance(context.grid.lines[n1], context.grid.lines[n2]);
}

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    const start = std.mem.indexOfScalar(u8, lines, 'S').?;
    const end = std.mem.indexOfScalar(u8, lines, 'E').?;

    var distances = std.AutoHashMap(usize, u32).init(allocator);
    defer distances.deinit();

    const search_context = SearchContext{ .grid = grid };
    try util.dijkstraSearch(usize, allocator, start, &distances, search_context, gridNeighbors, gridDistance);

    return distances.get(end).?;
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    var distances = std.AutoHashMap(usize, u32).init(allocator);
    defer distances.deinit();

    const end = std.mem.indexOfScalar(u8, lines, 'E').?;

    const search_context = SearchContext{ .grid = grid };
    try util.dijkstraSearch(usize, allocator, end, &distances, search_context, gridNeighbors, gridDistance);

    var start_idx: ?usize = std.mem.indexOfScalar(u8, lines, 'S');
    var min_dist: u32 = std.math.maxInt(u32);

    while (start_idx != null) : (start_idx = std.mem.indexOfScalarPos(u8, lines, start_idx.? + 1, 'S')) {
        min_dist = @min(min_dist, distances.get(start_idx.?).?);
    }

    return min_dist;
}

test "given example (part 1)" {
    const lines: []const u8 = "#######\n#6769##\nS50505E\n#97434#\n#######";
    try expectEqual(28, answer1(std.testing.allocator, lines));
}

test "given example (part 3)" {
    const lines: []const u8 = "SSSSSSSSSSS\nS674345621S\nS###6#4#18S\nS53#6#4532S\nS5450E0485S\nS##7154532S\nS2##314#18S\nS971595#34S\nSSSSSSSSSSS\n";
    try expectEqual(14, answer3(std.testing.allocator, lines));
}
