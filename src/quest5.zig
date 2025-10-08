const std = @import("std");
const expectEqual = std.testing.expectEqual;

// var debug_allocator = std.heap.DebugAllocator(.{}){};
// const allocator = debug_allocator.allocator();

const NodeU32 = struct {
    data: u32,
    node: std.DoublyLinkedList.Node = .{},
};

fn parseLists(lines: []const u8, allocator: std.mem.Allocator) ![4]std.DoublyLinkedList {
    var column_lists: [4]std.DoublyLinkedList = .{ .{}, .{}, .{}, .{} };

    var lines_it = std.mem.splitScalar(u8, lines, '\n');
    while (lines_it.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var line_it = std.mem.splitScalar(u8, line, ' ');
        for (0..4) |idx| {
            const num_str = line_it.next().?;
            const result = try std.fmt.parseInt(u32, num_str, 10);
            var node = try allocator.create(NodeU32);
            node.data = result;
            column_lists[idx].append(&node.node);
        }
    }
    return column_lists;
}

fn runClapper(column_lists: *[4]std.DoublyLinkedList, column_lengths: *[4]usize, curr_column: u8) void {
    const node = column_lists[curr_column].popFirst().?;
    column_lengths[curr_column] -= 1;
    const clapper_node: *NodeU32 = @fieldParentPtr("node", node);

    const next_column = (curr_column + 1) % 4;

    var curr_dancer = column_lists[next_column].first.?;
    var clap_num: u32 = 1;
    var left: bool = true;

    // skip all the roundabout until the end
    var clapper_value = clapper_node.data % (2 * column_lengths[next_column]);
    if (clapper_value == 0) {
        clapper_value = 2 * column_lengths[next_column];
    }
    while (clap_num != clapper_value) {
        const next_dancer = if (left) curr_dancer.next else curr_dancer.prev;
        if (next_dancer == null) {
            left = !left;
        } else {
            curr_dancer = next_dancer.?;
        }
        clap_num += 1;
    }
    // we're done with the clapping!  time to put ourself into the linked list
    // of course this is awful
    if (left) {
        // they go in front of curr_dancer (perhaps becoming the front of the list)
        if (curr_dancer.prev == null) {
            column_lists[next_column].prepend(node);
        } else {
            column_lists[next_column].insertBefore(curr_dancer, node);
        }
    } else {
        if (curr_dancer.next == null) {
            column_lists[next_column].append(node);
        } else {
            column_lists[next_column].insertAfter(curr_dancer, node);
        }
    }
    column_lengths[next_column] += 1;
}

fn bigNumber(column_lists: *const [4]std.DoublyLinkedList) u64 {
    var num: u64 = 0;
    for (0..4) |i| {
        const node: *NodeU32 = @fieldParentPtr("node", column_lists[i].first.?);
        const modulus: u64 = if (1000 <= node.data) 10000 else if (10 <= node.data) 100 else 10;
        num = (num * modulus) + node.data;
    }

    return num;
}

fn dealloc_list(l: *std.DoublyLinkedList, allocator: std.mem.Allocator) void {
    while (l.first != null) {
        const node = l.first.?;
        l.remove(node);
        const n: *NodeU32 = @fieldParentPtr("node", node);
        defer allocator.destroy(n);
    }
}

fn dealloc_lists(l: *[4]std.DoublyLinkedList, allocator: std.mem.Allocator) void {
    dealloc_list(&l[0], allocator);
    dealloc_list(&l[1], allocator);
    dealloc_list(&l[2], allocator);
    dealloc_list(&l[3], allocator);
}

test "bigNumber with two digits" {
    var lists = try parseLists("24 79 70 95\n", std.testing.allocator);
    try expectEqual(24797095, bigNumber(&lists));
    defer dealloc_lists(&lists, std.testing.allocator);

    var lists2 = try parseLists("10 10 10 10\n", std.testing.allocator);
    try expectEqual(10101010, bigNumber(&lists2));
    defer dealloc_lists(&lists2, std.testing.allocator);
}

test "bigNumber with four digits" {
    var lists = try parseLists("1005 1009 1008 8056\n", std.testing.allocator);
    defer dealloc_lists(&lists, std.testing.allocator);

    try expectEqual(1005100910088056, bigNumber(&lists));
}

fn printColumn(column: *const std.DoublyLinkedList) void {
    var it = column.first;
    while (it) |node| : (it = node.next) {
        const l: *NodeU32 = @fieldParentPtr("node", node);
        std.debug.print("{d}", .{l.data});
    }
    std.debug.print("\n", .{});
}

fn calculateColumnLengths(column_lists: [4]std.DoublyLinkedList) [4]usize {
    var result = [4]usize{ 0, 0, 0, 0 };
    for (0..4) |i| {
        result[i] = column_lists[i].len();
    }
    return result;
}

const ClappingIterator = struct {
    column_lists: [4]std.DoublyLinkedList,
    column_lengths: [4]usize,
    idx: u8,

    pub fn next(self: *ClappingIterator) u64 {
        runClapper(&self.column_lists, &self.column_lengths, self.idx);
        self.idx = (self.idx + 1) % 4;
        return bigNumber(&self.column_lists);
    }

    pub fn str(self: *ClappingIterator, allocator: std.mem.Allocator) ![]const u8 {
        const first: *NodeU32 = @fieldParentPtr("node", self.column_lists[0].first.?);
        const first_num = first.data;

        const num_capacity: u32 = if (first_num >= 1000) 4 else if (first_num >= 10) 2 else 1;
        const longest_length = @max(self.column_lengths[0], self.column_lengths[1], self.column_lengths[2], self.column_lengths[3]);
        var list = try std.ArrayList(u8).initCapacity(allocator, (num_capacity * 4 + 5) * longest_length);

        var nodes = [4]?*std.DoublyLinkedList.Node{
            self.column_lists[0].first,
            self.column_lists[1].first,
            self.column_lists[2].first,
            self.column_lists[3].first,
        };

        const list_writer = list.writer(allocator);

        try std.fmt.format(list_writer, "idx: {d}\n", .{self.idx});
        for (0..longest_length) |_| {
            var wrote_once = false;

            for (0..4) |k| {
                if (nodes[k] != null) {
                    const node: *NodeU32 = @fieldParentPtr("node", nodes[k].?);
                    try std.fmt.format(list_writer, "{d} ", .{node.data});
                    nodes[k] = nodes[k].?.next;
                    wrote_once = true;
                } else {
                    for (0..num_capacity + 1) |_| {
                        try std.fmt.format(list_writer, " ", .{});
                    }
                }
            }
            if (!wrote_once) {
                break;
            } else {
                try std.fmt.format(list_writer, "\n", .{});
            }
        }

        return list.items;
    }

    pub fn deinit(self: *ClappingIterator, allocator: std.mem.Allocator) void {
        dealloc_lists(&self.column_lists, allocator);
    }
};

fn createClappingIterator(lines: []const u8, allocator: std.mem.Allocator) !ClappingIterator {
    const column_lists = try parseLists(lines, allocator);
    return ClappingIterator{
        .column_lists = column_lists,
        .column_lengths = calculateColumnLengths(column_lists),
        .idx = 0,
    };
}

pub fn answer1(lines: []const u8, n: u32) !u64 {
    var it = try createClappingIterator(lines, std.heap.page_allocator);
    for (0..n - 1) |_| {
        _ = it.next();
    }
    return it.next();
}

test "given example" {
    const lines = "2 3 4 5\n3 4 5 2\n4 5 2 3\n5 2 3 4\n";
    var column_lists = try parseLists(lines, std.testing.allocator);
    defer dealloc_lists(&column_lists, std.testing.allocator);

    // not clear how to dealloc the linked lists (for now)
    printColumn(&column_lists[0]);
    printColumn(&column_lists[1]);
    printColumn(&column_lists[2]);
    printColumn(&column_lists[3]);

    var it = try createClappingIterator(lines, std.testing.allocator);
    defer it.deinit(std.testing.allocator);
    try expectEqual(3345, it.next());
    try expectEqual(3245, it.next());
    try expectEqual(3255, it.next());
    try expectEqual(3252, it.next());
    try expectEqual(4252, it.next());
    try expectEqual(4452, it.next());
    try expectEqual(4422, it.next());
    try expectEqual(4423, it.next());
    try expectEqual(2423, it.next());
    try expectEqual(2323, it.next());

    try expectEqual(3345, answer1(lines, 1));
    try expectEqual(3245, answer1(lines, 2));
    try expectEqual(3255, answer1(lines, 3));
    try expectEqual(2323, answer1(lines, 10));
}

pub fn answer2(lines: []const u8, allocator: std.mem.Allocator) !u64 {
    var map = std.AutoHashMap(u64, u64).init(
        std.heap.page_allocator,
    );
    defer map.deinit();

    var it = try createClappingIterator(lines, allocator);
    defer it.deinit(allocator);
    var round: u64 = 1;
    while (true) {
        const num = it.next();
        if (map.contains(num)) {
            const next_value = map.get(num).? + 1;
            if (next_value == 2024) {
                return round * num;
            }
            try map.put(num, next_value);
        } else {
            try map.put(num, 1);
        }
        round += 1;
    }

    return 0;
}

test "given example - part 2" {
    const lines = "2 3 4 5\n6 7 8 9\n";
    try expectEqual(50877075, answer2(lines, std.testing.allocator));
}

fn highestNumber(lines: []const u8, allocator: std.mem.Allocator) !u64 {
    // detecting loops - in Clojure I would just turn the current status
    // of the dance into a string, then find the point where it back "the same"
    // we can do something similar for Zig but it will be awful
    // I suppose it is awful time

    var i: u32 = 0;
    var map = std.StringHashMap(u32).init(allocator);
    defer map.deinit();

    var num_map = std.AutoHashMap(usize, u64).init(allocator);
    defer num_map.deinit();

    var it = try createClappingIterator(lines, allocator);
    while (true) {
        const num = it.next();
        const s = try it.str(allocator);

        if (map.contains(s)) {
            const start = map.get(s).?;
            std.debug.print("loop; started {d}, now {d}\n{s}\n", .{ start, i, s });

            var highest: u64 = 0;
            for (0..i) |n| {
                highest = @max(num_map.get(n).?, highest);
            }
            return highest;
        }
        try map.put(s, i);
        try num_map.put(i, num);
        i += 1;
    }
}

pub fn answer3(lines: []const u8, allocator: std.mem.Allocator) !u64 {
    return highestNumber(lines, allocator);
}

test "given example - part 3" {
    const lines = "2 3 4 5\n6 7 8 9\n";
    try expectEqual(6584, highestNumber(lines, std.testing.allocator) catch null);
}
