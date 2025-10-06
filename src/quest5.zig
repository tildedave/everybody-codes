const std = @import("std");

pub fn answer1(lines: []const u8) u32 {
    std.debug.print("{s}", .{lines});
    return "joe";
}

const NodeU32 = struct {
    data: u32,
    node: std.DoublyLinkedList.Node = .{},
};

test "given example" {
    const lines = "2 3 4 5\n3 4 5 2\n4 5 2 3\n5 2 3 4\n";
    var column_lists: [4]std.DoublyLinkedList = .{ .{}, .{}, .{}, .{} };
    // defer column_lists[0].deinit();
    // defer column_lists[1].deinit();
    // defer column_lists[2].deinit();
    // defer column_lists[3].deinit();

    var debug_allocator = std.heap.DebugAllocator(.{}){};
    const allocator = debug_allocator.allocator();

    var lines_it = std.mem.splitScalar(u8, lines, '\n');
    while (lines_it.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var line_it = std.mem.splitScalar(u8, line, ' ');
        for (0..4) |idx| {
            const num_str = line_it.next().?;
            const result = std.fmt.parseInt(u8, num_str, 10) catch null;
            var node = try allocator.create(NodeU32);
            node.data = result.?;
            column_lists[idx].append(&node.node);
        }
    }

    var it = column_lists[0].first;
    while (it) |node| : (it = node.next) {
        const l: *NodeU32 = @fieldParentPtr("node", node);
        std.debug.print("{d}\n", .{l.data});
    }
}
