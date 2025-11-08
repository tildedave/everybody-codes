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

fn rightRotationPermutation(allocator: std.mem.Allocator, grid: *util.Grid, idx: usize) ![]const usize {
    var permutation = try allocator.alloc(usize, grid.lines.len);

    const x, const y = util.coords(grid, idx);
    var i: usize = 0;
    while (i < grid.lines.len) : (i += 1) {
        // zig fmt: off
        permutation[i] =
            if (i == util.index(grid, x + 1, y)) util.index(grid, x + 1, y + 1)
            else if (i == util.index(grid, x + 1, y + 1)) util.index(grid, x, y + 1)
            else if (i == util.index(grid, x, y + 1)) util.index(grid, x - 1, y + 1)
            else if (i == util.index(grid, x - 1, y + 1)) util.index(grid, x - 1, y)
            else if (i == util.index(grid, x - 1, y)) util.index(grid, x - 1, y - 1)
            else if (i == util.index(grid, x - 1, y - 1)) util.index(grid, x, y - 1)
            else if (i == util.index(grid, x, y - 1)) util.index(grid, x + 1, y - 1)
            else if (i == util.index(grid, x + 1, y - 1)) util.index(grid, x + 1, y)
            else i;
        // zig fmt: on
    }

    return permutation;
}

fn leftRotationPermutation(allocator: std.mem.Allocator, grid: *util.Grid, idx: usize) ![]const usize {
    var permutation = try allocator.alloc(usize, grid.lines.len);

    const x, const y = util.coords(grid, idx);
    var i: usize = 0;
    while (i < grid.lines.len) : (i += 1) {
        // zig fmt: off
        permutation[i] =
            if (i == util.index(grid, x + 1, y)) util.index(grid, x + 1, y - 1)
            else if (i == util.index(grid, x + 1, y - 1)) util.index(grid, x, y - 1)
            else if (i == util.index(grid, x, y - 1)) util.index(grid, x - 1, y - 1)
            else if (i == util.index(grid, x - 1, y - 1)) util.index(grid, x - 1, y)
            else if (i == util.index(grid, x - 1, y)) util.index(grid, x - 1, y + 1)
            else if (i == util.index(grid, x - 1, y + 1)) util.index(grid, x, y + 1)
            else if (i == util.index(grid, x, y + 1)) util.index(grid, x + 1, y + 1)
            else if (i == util.index(grid, x + 1, y + 1)) util.index(grid, x + 1, y)
            else i;
        // zig fmt: on
    }
    return permutation;
}

fn composePermutations(dest: []usize, permutation1: []const usize, permutation2: []const usize) void {
    std.debug.assert(permutation1.len == permutation2.len);
    for (0..permutation1.len) |i| {
        dest[i] = permutation2[permutation1[i]];
    }
}

fn initialPermutation(dest: []usize, len: usize) void {
    for (0..len) |k| {
        dest[k] = k;
    }
}

fn expPermutation(allocator: std.mem.Allocator, dest: []usize, permutation: []const usize, n: usize) !void {
    const m = try allocator.alloc(usize, permutation.len);
    defer allocator.free(m);

    @memcpy(m, permutation);
    var _n = n;

    initialPermutation(dest, permutation.len);

    while (_n > 0) {
        if (_n % 2 == 1) {
            const next = try allocator.alloc(usize, permutation.len);
            defer allocator.free(next);
            composePermutations(next, m, dest);

            @memcpy(dest, next);
        }

        const next_m = try allocator.alloc(usize, permutation.len);
        defer allocator.free(next_m);
        composePermutations(next_m, m, m);
        @memcpy(m, next_m);

        _n = _n / 2;
    }
}

fn applyPermutation(allocator: std.mem.Allocator, grid: *util.Grid, permutation: []const usize) ![]const u8 {
    std.debug.assert(permutation.len == grid.lines.len);
    const result = try allocator.alloc(u8, grid.lines.len);
    for (0..grid.lines.len) |i| {
        result[permutation[i]] = grid.lines[i];
    }

    return result;
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

    const step_permutation = try allocator.alloc(usize, grid.lines.len);
    defer allocator.free(step_permutation);

    initialPermutation(step_permutation, grid.lines.len);

    var j: usize = 0;

    for (rotation_points.items) |rp| {
        const rotation_permutation = switch (instruction_line[j]) {
            'L' => try leftRotationPermutation(allocator, &grid, rp),
            'R' => try rightRotationPermutation(allocator, &grid, rp),
            else => unreachable,
        };
        defer allocator.free(rotation_permutation);

        const next = try allocator.alloc(usize, step_permutation.len);
        defer allocator.free(next);

        composePermutations(next, step_permutation, rotation_permutation);
        @memcpy(step_permutation, next);

        j = (j + 1) % (instruction_line.len);
    }

    const final_permutation = try allocator.alloc(usize, step_permutation.len);
    defer allocator.free(final_permutation);
    try expPermutation(allocator, final_permutation, step_permutation, times);

    const applied_grid = try applyPermutation(allocator, &grid, final_permutation);
    defer allocator.free(applied_grid);

    std.debug.print("ROTATED:\n{s}\n", .{applied_grid});

    const start_index = std.mem.indexOfScalar(u8, applied_grid, '>').?;
    const end_index = std.mem.indexOfScalar(u8, applied_grid, '<').?;
    _, const start_y = util.coords(&grid, start_index);
    _, const end_y = util.coords(&grid, end_index);
    std.debug.assert(start_y == end_y);

    const result = try allocator.alloc(u8, end_index - start_index - 1);
    @memcpy(result, applied_grid[start_index + 1 .. end_index]);
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
