const std = @import("std");
const util = @import("util.zig");
const expectEqual = std.testing.expectEqual;

fn dig(lines: []u8, next_lines: []u8, directions: []const util.Direction) u32 {
    var holes_dug: u32 = 0;
    const grid = util.createGrid(lines);
    for (0..grid.lines.len) |idx| {
        if (grid.lines[idx] == '\n') {
            next_lines[idx] = '\n';
            continue;
        }

        if (grid.lines[idx] == '#') {
            var it = util.NeighborIterator{ .grid = grid, .idx = idx, .directions = directions };
            var k: u8 = 0;
            while (it.next() == '#') {
                k += 1;
            }
            if (k == directions.len) {
                holes_dug += 1;
                next_lines[idx] = '#';
            } else {
                next_lines[idx] = '.';
            }
        } else {
            next_lines[idx] = '.';
        }
    }

    return holes_dug;
}

pub fn digArea(lines: []const u8, directions: []const util.Direction) !usize {
    const initial = std.mem.count(u8, lines, &[_]u8{'#'});

    var total = initial;
    var holes_dug = initial;

    const allocator = std.heap.page_allocator;
    var curr_lines = try allocator.alloc(u8, lines.len);
    var next_lines = try allocator.alloc(u8, lines.len);
    defer allocator.free(curr_lines);
    defer allocator.free(next_lines);

    std.mem.copyForwards(u8, curr_lines, lines);

    while (holes_dug != 0) {
        holes_dug = dig(curr_lines, next_lines, directions);
        total += holes_dug;

        const temp = curr_lines;
        curr_lines = next_lines;
        next_lines = temp;
    }

    return total;
}

pub fn answer1(lines: []const u8) !usize {
    return digArea(lines, util.cardinalDirections);
}

pub fn answer3(lines: []const u8) !usize {
    return digArea(lines, util.allDirections);
}

test "can remove" {
    const allocator = std.testing.allocator;

    const lines = "..........\n..###.##..\n...####...\n..######..\n..######..\n...####...\n..........\n";
    const mut_lines: []u8 = try allocator.alloc(u8, lines.len);
    defer allocator.free(mut_lines);

    std.debug.print("{s}", .{lines});

    std.mem.copyForwards(u8, mut_lines, lines);
    const next_lines: []u8 = try allocator.alloc(u8, lines.len);
    defer allocator.free(next_lines);
}

test "given example (part 1)" {
    const lines = "..........\n..###.##..\n...####...\n..######..\n..######..\n...####...\n..........\n";
    try expectEqual(35, answer1(lines));
}

test "given example (part 3)" {
    const lines = "..........\n..###.##..\n...####...\n..######..\n..######..\n...####...\n..........\n";
    try expectEqual(29, answer3(lines));
}
