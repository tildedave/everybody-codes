const std = @import("std");
const expectEqual = std.testing.expectEqual;

pub fn nthIndexOfScalar(comptime T: type, slice: []const T, value: T, n: usize) ?usize {
    var pos: usize = 0;
    for (0..n + 1) |idx| {
        const start_pos = if (idx == 0) pos else pos + 1;
        const next_pos = std.mem.indexOfScalarPos(T, slice, start_pos, value);
        if (next_pos == null) {
            return null;
        }
        pos = next_pos.?;
    }

    return pos;
}

test "nthIndexOfScalar" {
    try expectEqual(0, nthIndexOfScalar(u8, "aaaa", 'a', 0));
    try expectEqual(1, nthIndexOfScalar(u8, "aaaa", 'a', 1));
    try expectEqual(2, nthIndexOfScalar(u8, "aaaa", 'a', 2));
    try expectEqual(3, nthIndexOfScalar(u8, "aaaa", 'a', 3));
}

pub const Grid = struct {
    lines: []const u8,
    width: usize,
    height: usize,
};

pub const Direction = enum { up, down, left, right, upleft, upright, downleft, downright };

pub const cardinalDirections: []const Direction = &[_]Direction{ .up, .down, .left, .right };
pub const allDirections: []const Direction = &[_]Direction{ .up, .upright, .right, .downright, .down, .downleft, .left, .upleft };

pub const WalkOptions = struct {
    wraparound_horizontal: bool = false,
    wraparound_vertical: bool = false,
};

fn up_y(grid: Grid, y: usize, opts: WalkOptions) ?usize {
    if (y == 0) {
        if (!opts.wraparound_vertical) {
            return null;
        }

        return grid.height - 1;
    } else {
        return y - 1;
    }
}

fn down_y(grid: Grid, y: usize, opts: WalkOptions) ?usize {
    if (y == grid.height - 1) {
        if (!opts.wraparound_vertical) {
            return null;
        }
        return 0;
    } else {
        return (y + 1);
    }
}

fn left_x(grid: Grid, x: usize, opts: WalkOptions) ?usize {
    if (x == 0) {
        if (opts.wraparound_horizontal) {
            return grid.width - 2;
        } else {
            return null;
        }
    } else {
        return x - 1;
    }
}

fn right_x(grid: Grid, x: usize, opts: WalkOptions) ?usize {
    if (x == grid.width - 2 and !opts.wraparound_horizontal) {
        return null;
    }

    return (x + 1) % (grid.width - 1);
}

pub fn walk(grid: Grid, idx: usize, direction: Direction, opts: WalkOptions) ?usize {
    const x = idx % grid.width;
    const y = idx / grid.width;

    var next_y: ?usize = y;
    var next_x: ?usize = x;

    switch (direction) {
        .up => next_y = up_y(grid, y, opts),
        .down => next_y = down_y(grid, y, opts),
        .left => next_x = left_x(grid, x, opts),
        .right => next_x = right_x(grid, x, opts),
        .upleft => {
            next_x = left_x(grid, x, opts);
            next_y = up_y(grid, y, opts);
        },
        .upright => {
            next_x = right_x(grid, x, opts);
            next_y = up_y(grid, y, opts);
        },
        .downleft => {
            next_x = left_x(grid, x, opts);
            next_y = down_y(grid, y, opts);
        },
        .downright => {
            next_x = right_x(grid, x, opts);
            next_y = down_y(grid, y, opts);
        },
    }

    if (next_x == null or next_y == null) {
        return null;
    }

    return next_y.? * grid.width + next_x.?;
}

// infinite iterator
pub const DirectionIterator = struct {
    grid: Grid,
    direction: Direction,
    idx: usize,
    is_first: bool = true,

    walk_opts: WalkOptions = .{ .wraparound_horizontal = false, .wraparound_vertical = false },

    pub fn next(self: *DirectionIterator) ?u8 {
        if (self.is_first) {
            self.is_first = false;
        } else {
            const next_idx = walk(self.grid, self.idx, self.direction, self.walk_opts);
            if (next_idx == null) {
                return null;
            } else {
                self.idx = next_idx.?;
            }
        }

        return self.grid.lines[self.idx];
    }
};

pub fn createGrid(lines: []const u8) Grid {
    return Grid{
        .lines = lines,
        .width = std.mem.indexOfScalar(u8, lines, '\n').? + 1,
        .height = std.mem.count(u8, lines, "\n"),
    };
}

pub const NeighborIterator = struct {
    grid: Grid,
    idx: usize,
    walk_opts: WalkOptions = .{ .wraparound_horizontal = false, .wraparound_vertical = false },
    directions: []const Direction = &[_]Direction{ .up, .right, .down, .left },
    dir_idx: usize = 0,

    pub fn next(self: *NeighborIterator) ?u8 {
        while (self.dir_idx < self.directions.len) {
            const dir = self.directions[self.dir_idx];
            const next_idx = walk(self.grid, self.idx, dir, self.walk_opts);
            self.dir_idx += 1;

            if (next_idx != null) {
                return self.grid.lines[next_idx.?];
            }
        }
        return null;
    }
};

test "NeighborIterator" {
    const lines = "..........\n..###.##..\n...####...\n..######..\n..######..\n...####...\n..........\n";
    const grid = createGrid(lines);
    var it = NeighborIterator{ .grid = grid, .idx = 13 };
    try expectEqual('#', grid.lines[13]);
    try expectEqual('.', it.next());
    try expectEqual('#', it.next());
    try expectEqual('.', it.next());
    try expectEqual('.', it.next());
    try expectEqual(null, it.next());
}

test "NeighborIterator (all neighbors)" {
    const lines = "..........\n..###.##..\n...####...\n..######..\n..######..\n...####...\n..........\n";
    const grid = createGrid(lines);
    var it = NeighborIterator{ .grid = grid, .idx = 13, .directions = allDirections };
    try expectEqual('#', grid.lines[13]);
    try expectEqual('.', it.next());
    try expectEqual('.', it.next());
    try expectEqual('#', it.next());
    try expectEqual('#', it.next());
    try expectEqual('.', it.next());
    try expectEqual('.', it.next());
    try expectEqual('.', it.next());
    try expectEqual('.', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - right (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .right, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = true } };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('L', it.next());
    try expectEqual('W', it.next());
    try expectEqual('O', it.next());
    try expectEqual('R', it.next());
    try expectEqual('L', it.next());
    try expectEqual('T', it.next());
    try expectEqual('H', it.next());
}

test "DirectionIterator - right (no wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .right, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = false } };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('L', it.next());
    try expectEqual('W', it.next());
    try expectEqual('O', it.next());
    try expectEqual('R', it.next());
    try expectEqual('L', it.next());
    try expectEqual('T', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - down (no wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .down, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = true } };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('T', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - down (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .down, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_vertical = true } };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('T', it.next());
    try expectEqual('H', it.next());
}

test "DirectionIterator - left (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .left, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = true } };
    try expectEqual('H', it.next());
    try expectEqual('T', it.next());
    try expectEqual('L', it.next());
}

test "DirectionIterator - left (no wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .left, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = false } };
    try expectEqual('H', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - up (no wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);

    var it = DirectionIterator{ .direction = .up, .idx = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - up (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);

    var it = DirectionIterator{ .direction = .up, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_vertical = true } };
    try expectEqual('H', it.next());
    try expectEqual('T', it.next());
    try expectEqual('E', it.next());
}

pub const NumberIterator = struct {
    lines: []const u8,
    lines_iterator: ?std.mem.SplitIterator(u8, .scalar) = null,

    pub fn next(self: *NumberIterator) ?u32 {
        if (self.lines_iterator == null) {
            self.lines_iterator = std.mem.splitScalar(u8, self.lines, '\n');
        }
        const num_str = self.lines_iterator.?.next();
        if (num_str == null) {
            return null;
        }

        const result = std.fmt.parseInt(u32, num_str.?, 10) catch null;
        return result;
    }
};

test "NumberIterator" {
    const lines = "14\n15\n9\n10024\n";
    var it = NumberIterator{ .lines = lines };
    try expectEqual(14, it.next());
    try expectEqual(15, it.next());
    try expectEqual(9, it.next());
    try expectEqual(10024, it.next());
}
