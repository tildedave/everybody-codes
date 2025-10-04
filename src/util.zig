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

pub const Direction = enum { up, down, left, right };

pub const WalkOptions = struct {
    wraparound_horizontal: bool = false,
    wraparound_vertical: bool = false,
};

pub fn walk(grid: Grid, idx: usize, direction: Direction, opts: WalkOptions) ?usize {
    var x = idx % grid.width;
    var y = idx / grid.width;

    switch (direction) {
        .up => {
            if (y == 0) {
                if (!opts.wraparound_vertical) {
                    return null;
                }

                y = grid.height - 1;
            } else {
                y = y - 1;
            }
        },
        .down => {
            if (y == grid.height - 1) {
                if (!opts.wraparound_vertical) {
                    return null;
                }
                y = 0;
            } else {
                y = (y + 1);
            }
        },
        .left => {
            if (x == 0) {
                if (opts.wraparound_horizontal) {
                    x = grid.width - 2;
                } else {
                    return null;
                }
            } else {
                x = x - 1;
            }
        },
        .right => {
            if (x == grid.width - 2 and !opts.wraparound_horizontal) {
                return null;
            }

            x = (x + 1) % (grid.width - 1);
        },
    }

    return y * grid.width + x;
}

// infinite iterator
pub const DirectionIterator = struct {
    grid: Grid,
    direction: Direction,
    idx: usize,
    is_first: bool = true,

    walk_opts: WalkOptions = .{ .wraparound_horizontal = true, .wraparound_vertical = false },

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
        .height = std.mem.count(u8, lines, &[_]u8{'\n'}),
    };
}

test "DirectionIterator - right (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = createGrid(lines);
    var it = DirectionIterator{ .direction = .right, .idx = 0, .grid = grid };
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
    var it = DirectionIterator{ .direction = .down, .idx = 0, .grid = grid };
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
