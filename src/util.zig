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

// infinite iterator
pub const DirectionIterator = struct {
    grid: Grid,
    direction: Direction,
    idx: usize,
    is_first: bool = true,

    wraps_left_to_right: bool = true,

    fn walk(self: *DirectionIterator) bool {
        var x = self.idx % self.grid.width;
        var y = self.idx / self.grid.width;

        switch (self.direction) {
            .up => {
                if (y == 0) {
                    return false;
                } else {
                    y = y - 1;
                }
            },
            .down => {
                if (y == self.grid.height - 1) {
                    return false;
                }
                y = (y + 1);
            },
            .left => {
                if (x == 0) {
                    if (self.wraps_left_to_right) {
                        x = self.grid.width - 2;
                    } else {
                        return false;
                    }
                } else {
                    x = x - 1;
                }
            },
            .right => {
                if (x == self.grid.width - 2 and !self.wraps_left_to_right) {
                    return false;
                }

                x = (x + 1) % (self.grid.width - 1);
            },
        }

        self.idx = y * self.grid.width + x;
        return true;
    }

    pub fn next(self: *DirectionIterator) ?u8 {
        if (self.is_first) {
            self.is_first = false;
        } else if (!self.walk()) {
            return null;
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

test "DirectionIterator" {
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
    it = DirectionIterator{ .direction = .down, .idx = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('T', it.next());
    try expectEqual(null, it.next());
    it = DirectionIterator{ .direction = .left, .idx = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual('T', it.next());
    try expectEqual('L', it.next());
    it = DirectionIterator{ .direction = .up, .idx = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual(null, it.next());
}
