const std = @import("std");
const expectEqualStrings = std.testing.expectEqualStrings;
const util = @import("util.zig");

fn rotateRight(grid: *util.Grid, i: usize) void {
    const x, const y = util.coords(grid, i);
    // start at x+1 I guess
    const temp = grid.lines[util.index(grid, x + 1, y)];
    grid.lines[util.index(grid, x + 1, y)] = grid.lines[util.index(grid, x + 1, y - 1)];
    grid.lines[util.index(grid, x + 1, y - 1)] = grid.lines[util.index(grid, x, y - 1)];
    grid.lines[util.index(grid, x, y - 1)] = grid.lines[util.index(grid, x - 1, y - 1)];
    grid.lines[util.index(grid, x - 1, y - 1)] = grid.lines[util.index(grid, x - 1, y)];
    grid.lines[util.index(grid, x - 1, y)] = grid.lines[util.index(grid, x - 1, y + 1)];
    grid.lines[util.index(grid, x - 1, y + 1)] = grid.lines[util.index(grid, x, y + 1)];
    grid.lines[util.index(grid, x, y + 1)] = grid.lines[util.index(grid, x + 1, y + 1)];
    grid.lines[util.index(grid, x + 1, y + 1)] = temp;
}

fn rotateLeft(grid: *util.Grid, i: usize) void {
    const x, const y = util.coords(grid, i);

    const temp = grid.lines[util.index(grid, x + 1, y)];
    grid.lines[util.index(grid, x + 1, y)] = grid.lines[util.index(grid, x + 1, y + 1)];
    grid.lines[util.index(grid, x + 1, y + 1)] = grid.lines[util.index(grid, x, y + 1)];
    grid.lines[util.index(grid, x, y + 1)] = grid.lines[util.index(grid, x - 1, y + 1)];
    grid.lines[util.index(grid, x - 1, y + 1)] = grid.lines[util.index(grid, x - 1, y)];
    grid.lines[util.index(grid, x - 1, y)] = grid.lines[util.index(grid, x - 1, y - 1)];
    grid.lines[util.index(grid, x - 1, y - 1)] = grid.lines[util.index(grid, x, y - 1)];
    grid.lines[util.index(grid, x, y - 1)] = grid.lines[util.index(grid, x + 1, y - 1)];
    grid.lines[util.index(grid, x + 1, y - 1)] = temp;
}

pub fn answer(allocator: std.mem.Allocator, lines: []const u8, times: usize) ![]const u8 {
    const newline_pos = std.mem.indexOfScalar(u8, lines, '\n').?;
    const instruction_line = lines[0..newline_pos];
    const grid_start_pos = newline_pos + 2;
    var grid = try util.createGrid(allocator, lines[grid_start_pos..]);
    defer allocator.free(grid.lines);

    var rotation_points = try std.ArrayList(usize).initCapacity(allocator, 0);
    defer rotation_points.deinit(allocator);

    var i: usize = 0;
    while (i < grid.lines.len) : (i += 1) {
        const x, const y = util.coords(&grid, i);
        if (x > 0 and x < grid.width - 1 and y > 0 and y < grid.height - 1) {
            try rotation_points.append(allocator, i);
        }
    }

    for (0..times) |loop_num| {
        if (loop_num % 1000 == 0) {
            std.debug.print("{d}\n", .{loop_num});
        }
        var j: usize = 0;
        for (rotation_points.items) |rp| {
            switch (instruction_line[j]) {
                'L' => {
                    rotateLeft(&grid, rp);
                },
                'R' => {
                    rotateRight(&grid, rp);
                },
                else => unreachable,
            }
            j = (j + 1) % (instruction_line.len);
        }
    }

    const start_index = std.mem.indexOfScalar(u8, grid.lines, '>').?;
    const end_index = std.mem.indexOfScalar(u8, grid.lines, '<').?;
    _, const start_y = util.coords(&grid, start_index);
    _, const end_y = util.coords(&grid, end_index);
    std.debug.assert(start_y == end_y);

    const result = try allocator.alloc(u8, end_index - start_index - 1);
    @memcpy(result, grid.lines[start_index + 1 .. end_index]);
    return result;
}

test "given example (part 1)" {
    const lines = "LR\n\n>-IN-\n-----\nW---<\n";
    const result = try answer(std.testing.allocator, lines, 1);
    defer std.testing.allocator.free(result);
    try expectEqualStrings("WIN", result);
}

test "given example (part 2)" {
    const lines = "RRLL\n\nA.VI..>...T\n.CC...<...O\n.....EIB.R.\n.DHB...YF..\n.....F..G..\nD.H........\n";
    const result = try answer(std.testing.allocator, lines, 100);
    defer std.testing.allocator.free(result);
    try expectEqualStrings("VICTORY", result);
}
