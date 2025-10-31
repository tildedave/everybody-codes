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

pub fn answer1(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    const start = std.mem.indexOfScalar(u8, lines, 'S').?;
    return dijkstraSearch(allocator, grid, start);
}

pub fn answer3(allocator: std.mem.Allocator, lines: []const u8) !u64 {
    const grid = try util.createGrid(allocator, lines);
    defer allocator.free(grid.lines);

    // checking everything is certainly overkill but whatever
    var idx: usize = 0;
    var min_search: u64 = std.math.maxInt(u64);
    while (std.mem.indexOfScalarPos(u8, lines, idx, 'S')) |start| : (idx = start + 1) {
        const d = try dijkstraSearch(allocator, grid, start);
        if (d < min_search) {
            std.debug.print("{d}, {d}\n", .{ start, d });
            min_search = d;
        }
    }
    return min_search;
}

fn dijkstraSearch(allocator: std.mem.Allocator, grid: util.Grid, start: usize) !u64 {
    var distances = std.AutoHashMap(usize, u64).init(allocator);
    defer distances.deinit();
    var visited = std.AutoHashMap(usize, bool).init(allocator);
    defer visited.deinit();
    var frontier = std.AutoHashMap(usize, bool).init(allocator);
    defer frontier.deinit();

    const end = std.mem.indexOfScalar(u8, grid.lines, 'E').?;

    try frontier.put(start, true);
    try distances.put(start, 0);

    while (frontier.count() > 0) {
        var frontier_it = frontier.iterator();

        var node_dist: u64 = std.math.maxInt(u64);
        var node: ?usize = null;
        while (frontier_it.next()) |e| {
            if (distances.get(e.key_ptr.*)) |d| {
                if (d < node_dist) {
                    node = e.key_ptr.*;
                    node_dist = d;
                }
            }
        }

        try visited.put(node.?, true);
        _ = frontier.remove(node.?);

        if (node == end) {
            return node_dist;
        }

        const node_level = grid.lines[node.?];

        // otherwise look at the neighbors
        var it = util.NeighborIterator{ .grid = grid, .idx = node.? };
        while (it.next()) |ni| {
            if (ni == '#' or ni == ' ') {
                continue;
            }

            const d = distance(ni, node_level);

            if (visited.contains(it.next_idx)) {
                continue;
            }

            try frontier.put(it.next_idx, true);
            if (distances.get(it.next_idx)) |neighbor_dist| {
                if (d + node_dist < neighbor_dist) {
                    distances.putAssumeCapacity(it.next_idx, d + node_dist);
                }
            } else {
                try distances.put(it.next_idx, d + node_dist);
            }
        }
    }

    return SearchError.Unreachable;
}

test "given example (part 1)" {
    const lines: []const u8 = "#######\n#6769##\nS50505E\n#97434#\n#######";
    try expectEqual(28, answer1(std.testing.allocator, lines));
}

test "given example (part 3)" {
    const lines: []const u8 = "SSSSSSSSSSS\nS674345621S\nS###6#4#18S\nS53#6#4532S\nS5450E0485S\nS##7154532S\nS2##314#18S\nS971595#34S\nSSSSSSSSSSS\n";
    try expectEqual(14, answer3(std.testing.allocator, lines));
}
