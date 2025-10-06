const std = @import("std");
const expectEqual = std.testing.expectEqual;

var debug_allocator = std.heap.DebugAllocator(.{}){};
const allocator = debug_allocator.allocator();

const NodeU32 = struct {
    data: u32,
    node: std.DoublyLinkedList.Node = .{},
};

fn parseLists(lines: []const u8) ![4]std.DoublyLinkedList {
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
    const clapper_value = clapper_node.data;

    const next_column = (curr_column + 1) % 4;

    var curr_dancer = column_lists[next_column].first.?;
    var clap_num: u32 = 1;
    var left: bool = true;

    // naive; we should be able to make this smarter
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

test "bigNumber with two digits" {
    var lists = try parseLists("24 79 70 95\n");
    try expectEqual(24797095, bigNumber(&lists));

    lists = try parseLists("10 10 10 10\n");
    try expectEqual(10101010, bigNumber(&lists));
}

test "bigNumber with four digits" {
    var lists = try parseLists("1005 1009 1008 8056\n");
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
};

fn createClappingIterator(lines: []const u8) !ClappingIterator {
    const column_lists = try parseLists(lines);
    return ClappingIterator{
        .column_lists = column_lists,
        .column_lengths = calculateColumnLengths(column_lists),
        .idx = 0,
    };
}

pub fn answer1(lines: []const u8, n: u32) !u64 {
    var it = try createClappingIterator(lines);
    for (0..n - 1) |_| {
        _ = it.next();
    }
    return it.next();
}

test "given example" {
    const lines = "2 3 4 5\n3 4 5 2\n4 5 2 3\n5 2 3 4\n";
    var column_lists = try parseLists(lines);
    // not clear how to dealloc the linked lists (for now)
    printColumn(&column_lists[0]);
    printColumn(&column_lists[1]);
    printColumn(&column_lists[2]);
    printColumn(&column_lists[3]);

    var it = try createClappingIterator(lines);
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

pub fn answer2(lines: []const u8) !u64 {
    var map = std.AutoHashMap(u64, u64).init(
        allocator,
    );
    defer map.deinit();

    var it = try createClappingIterator(lines);
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
        // std.debug.print("round {d} num {d}\n", .{ round, num });
    }

    return 0;
}

test "given example - part 2" {
    const lines = "2 3 4 5\n6 7 8 9\n";
    try expectEqual(50877075, answer2(lines));
}
