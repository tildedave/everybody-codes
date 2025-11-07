const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

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
    lines: []u8,
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
            return grid.width - 1;
        } else {
            return null;
        }
    } else {
        return x - 1;
    }
}

fn right_x(grid: Grid, x: usize, opts: WalkOptions) ?usize {
    if (x == grid.width - 1 and !opts.wraparound_horizontal) {
        return null;
    }

    return (x + 1) % grid.width;
}

pub fn index(grid: *const Grid, x: usize, y: usize) usize {
    return y * (grid.width + 1) + x;
}

pub fn coords(grid: *const Grid, idx: usize) struct { usize, usize } {
    return .{ idx % (grid.width + 1), idx / (grid.width + 1) };
}

pub fn walk(grid: Grid, idx: usize, direction: Direction, opts: WalkOptions) ?usize {
    const x, const y = coords(&grid, idx);

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

    return index(&grid, next_x.?, next_y.?);
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

pub fn createGrid(allocator: std.mem.Allocator, lines: []const u8) !Grid {
    const lines_copy = try allocator.alloc(u8, lines.len);
    @memcpy(lines_copy, lines);

    return Grid{
        .lines = lines_copy,
        .width = std.mem.indexOfScalar(u8, lines, '\n').?,
        .height = std.mem.count(u8, lines, "\n"),
    };
}

pub const NeighborIterator = struct {
    grid: Grid,
    idx: usize,
    walk_opts: WalkOptions = .{ .wraparound_horizontal = false, .wraparound_vertical = false },
    directions: []const Direction = &[_]Direction{ .up, .right, .down, .left },
    dir_idx: usize = 0,
    next_idx: usize = 0,

    pub fn next(self: *NeighborIterator) ?u8 {
        while (self.dir_idx < self.directions.len) {
            const dir = self.directions[self.dir_idx];
            const next_idx = walk(self.grid, self.idx, dir, self.walk_opts);
            self.dir_idx += 1;

            if (next_idx) |ni| {
                self.next_idx = ni;
                return self.grid.lines[ni];
            }
        }
        return null;
    }
};

test "NeighborIterator" {
    const lines = "..........\n..###.##..\n...####...\n..######..\n..######..\n...####...\n..........\n";
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

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
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

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
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

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
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

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
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

    var it = DirectionIterator{ .direction = .down, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = true } };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('T', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - down (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

    var it = DirectionIterator{ .direction = .down, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_vertical = true } };
    try expectEqual('H', it.next());
    try expectEqual('E', it.next());
    try expectEqual('T', it.next());
    try expectEqual('H', it.next());
}

test "DirectionIterator - left (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

    var it = DirectionIterator{ .direction = .left, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = true } };
    try expectEqual('H', it.next());
    try expectEqual('T', it.next());
    try expectEqual('L', it.next());
}

test "DirectionIterator - left (no wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

    var it = DirectionIterator{ .direction = .left, .idx = 0, .grid = grid, .walk_opts = .{ .wraparound_horizontal = false } };
    try expectEqual('H', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - up (no wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

    var it = DirectionIterator{ .direction = .up, .idx = 0, .grid = grid };
    try expectEqual('H', it.next());
    try expectEqual(null, it.next());
}

test "DirectionIterator - up (wraparound)" {
    const lines = "HELWORLT\nENIGWDXL\nTRODEOAL\n";
    const grid = try createGrid(std.testing.allocator, lines);
    defer std.testing.allocator.free(grid.lines);

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

pub fn generatePermutations(
    comptime T: type,
    l: []T,
    ctx: anytype,
    comparator: *const fn (lhs: T, rhs: T) i8,
    processFn: *const fn (@TypeOf(ctx), str: []const T) void,
) void {
    processFn(ctx, l);

    while (true) {
        var i: usize = l.len - 2;
        var found: bool = true;
        while (i >= 0 and comparator(l[i], l[i + 1]) >= 0) {
            if (i == 0) {
                found = false;
                break;
            }
            i -= 1;
        }
        if (!found) {
            break;
        }

        var j = l.len - 1;
        while (j > i and comparator(l[j], l[i]) != 1) : (j -= 1) {}

        var tmp: T = l[j];
        l[j] = l[i];
        l[i] = tmp;

        // reverse the list after l[i+1]
        var curr = i + 1;
        var next = l.len - 1;
        while (curr < next) {
            tmp = l[curr];
            l[curr] = l[next];
            l[next] = tmp;
            curr += 1;
            next -= 1;
        }

        processFn(ctx, l);
    }
}

const Counter = struct {
    total: u32,
};

fn countFn(ctx: *Counter, str: []const u8) void {
    ctx.total += 1;
    if (str.len > 0) {}
    // std.debug.print("{s}\n", .{str});
}

fn compareU8(lhs: u8, rhs: u8) i8 {
    if (lhs < rhs) {
        return -1;
    } else if (lhs == rhs) {
        return 0;
    } else if (lhs > rhs) {
        return 1;
    }
    unreachable;
}

test "generatePermutations" {
    var v: Counter = .{ .total = 0 };
    var l = [5]u8{ 1, 1, 2, 3, 4 };
    generatePermutations(u8, &l, &v, compareU8, &countFn);
    try expectEqual(60, v.total);
}

const SearchError = error{Unreachable};

pub fn Searcher(comptime T: type) type {
    return struct {
        const Self = @This();
        const PriorityQueueContext = struct { distances: *std.AutoHashMap(T, u32) };

        pub fn init() Self {
            return .{};
        }

        fn comparator(ctx: PriorityQueueContext, a: T, b: T) std.math.Order {
            return std.math.order(ctx.distances.get(a).?, ctx.distances.get(b).?);
        }

        pub fn dijkstra(
            _: *Searcher(T),
            allocator: std.mem.Allocator,
            start: T,
            distances: *std.AutoHashMap(T, u32),
            context: anytype,
            neighbors: fn (node: T, @TypeOf(allocator), *std.ArrayList(T), @TypeOf(context)) error{OutOfMemory}!void,
            distance: fn (node1: T, node2: T, @TypeOf(context)) u32,
        ) !void {
            var visited = std.AutoHashMap(T, bool).init(allocator);
            defer visited.deinit();

            const Queue = std.PriorityQueue(T, PriorityQueueContext, comparator);
            var frontier = Queue.init(allocator, .{ .distances = distances });
            defer frontier.deinit();

            try distances.put(start, 0);
            try frontier.add(start);

            while (frontier.count() > 0) {
                const node = frontier.remove();
                const node_dist = distances.get(node).?;

                try visited.put(node, true);

                var neighbor_list = try std.ArrayList(T).initCapacity(allocator, 0);
                defer neighbor_list.deinit(allocator);
                try neighbors(node, allocator, &neighbor_list, context);

                for (0..neighbor_list.items.len) |i| {
                    const n: T = neighbor_list.items[i];
                    if (visited.contains(n)) {
                        continue;
                    }

                    const d = distance(n, node, context);
                    if (distances.get(n)) |neighbor_dist| {
                        if (node_dist + d < neighbor_dist) {
                            distances.putAssumeCapacity(n, node_dist + d);
                            // this is O(n) to find n unfortunately
                            try frontier.update(n, n);
                        }
                    } else {
                        try distances.put(n, node_dist + d);
                        try frontier.add(n);
                    }
                }
            }
        }

        pub fn astar(
            _: *Searcher(T),
            allocator: std.mem.Allocator,
            start: T,
            distances: *std.AutoHashMap(T, u32),
            context: anytype,
            neighbors: fn (node: T, @TypeOf(allocator), *std.ArrayList(T), @TypeOf(context)) error{OutOfMemory}!void,
            distance: fn (node1: T, node2: T, @TypeOf(context)) u32,
            heuristic: fn (node: T, @TypeOf(context)) u32,
            isGoal: fn (node: T, @TypeOf(context)) bool,
            shouldPrune: fn (node: T, @TypeOf(context)) bool,
        ) !void {
            var guess_distances = std.AutoHashMap(T, u32).init(allocator);
            defer guess_distances.deinit();

            const Queue = std.PriorityQueue(T, PriorityQueueContext, comparator);
            var frontier = Queue.init(allocator, .{ .distances = &guess_distances });
            defer frontier.deinit();

            try distances.put(start, 0);
            try frontier.add(start);
            try guess_distances.put(start, heuristic(start, context));

            while (frontier.count() > 0) {
                const node = frontier.remove();

                if (isGoal(node, context)) {
                    std.debug.print("found goal {any} {d}\n", .{ node, distances.get(node).? });
                    return;
                }

                if (shouldPrune(node, context)) {
                    continue;
                }

                const node_dist = distances.get(node).?;

                var neighbor_list = try std.ArrayList(T).initCapacity(allocator, 0);
                defer neighbor_list.deinit(allocator);
                try neighbors(node, allocator, &neighbor_list, context);

                for (0..neighbor_list.items.len) |i| {
                    const n: T = neighbor_list.items[i];
                    const score = node_dist + distance(node, n, context);

                    if (score < distances.get(n) orelse std.math.maxInt(u32)) {
                        const is_new = !distances.contains(n);
                        try distances.put(n, score);
                        try guess_distances.put(n, score + heuristic(n, context));
                        if (is_new) {
                            try frontier.add(n);
                        } else {
                            try frontier.update(n, n);
                        }
                    }
                }
            }

            return SearchError.Unreachable;
        }
    };
}

pub fn DisjointSet(comptime T: type) type {
    return struct {
        pub const Node = struct {
            elem: T,
            parent: ?*Node,
        };

        items: std.ArrayList(Node),
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) !Self {
            return .{
                .items = try std.ArrayList(Node).initCapacity(allocator, 0),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.items.deinit(self.allocator);
        }

        pub fn makeSet(self: *Self, elem: T) !*Node {
            const list = Node{ .elem = elem, .parent = null };
            try self.items.append(self.allocator, list);

            return &self.items.items[self.items.items.len - 1];
        }

        pub fn find(_: Self, elem: *Node) *Node {
            var root = elem;
            while (root.parent != null) : (root = root.parent.?) {}

            var node = elem;
            while (node.parent != null) {
                const next_parent = node.parent.?;
                node.parent = root;
                node = next_parent;
            }

            return root;
        }

        pub fn unionSets(self: *Self, elem1: *Node, elem2: *Node) void {
            var elem1_root = self.find(elem1);
            const elem2_root = self.find(elem2);
            std.debug.assert(elem1_root.parent == null);
            std.debug.assert(elem2_root.parent == null);

            elem1_root.parent = elem2_root;
        }
    };
}

test "union find" {
    var set = try DisjointSet(u32).init(std.testing.allocator);
    defer set.deinit();

    const e1 = try set.makeSet(1);
    const e2 = try set.makeSet(2);

    try expect(set.find(e1).elem != set.find(e2).elem);
    set.unionSets(e1, e2);

    try expectEqual(set.find(e1).elem, set.find(e2).elem);
}
