const std = @import("std");
const expectEqual = std.testing.expectEqual;

pub fn answer1(num: u64) u64 {
    // 1 + 3 + 5 + 7 + ... = n^2
    var i: u64 = 0;
    while (i * i < num) : (i += 1) {}
    return (2 * i - 1) * (i * i - num);
}

test "given example" {
    try expectEqual(21, answer1(13));
}

pub fn answer2(blocks: u64, num_acolytes: u64, num_priests: u64) u64 {
    // this looks more like a recurrence
    // seems clear we still don't want to simulate this :-)
    // blocks required per layer =

    // thickness = (prev_thickness * priests) mod priests
    // (2 * i - 1) * thickness

    var prev_thickness: u64 = 1;
    var i: u32 = 2;
    var blocks_left = blocks - 1;
    while (true) {
        const thickness = (prev_thickness * num_priests) % num_acolytes;
        const blocks_needed = (2 * i - 1) * thickness;
        if (blocks_needed > blocks_left) {
            return (blocks_needed - blocks_left) * (2 * i - 1);
        }
        blocks_left -= blocks_needed;
        prev_thickness = thickness;
        i += 1;
    }
}

test "given example (part 2)" {
    try expectEqual(27, answer2(50, 5, 3));
}

const NodeU64 = struct {
    data: u64,
    node: std.DoublyLinkedList.Node = .{},
};

pub fn answer3(allocator: std.mem.Allocator, num_blocks: u64, num_acolytes: u64, num_priests: u64) !u64 {
    var list: std.DoublyLinkedList = .{};
    var node = try allocator.create(NodeU64);
    defer {
        while (list.first) |n| {
            list.remove(n);
            const curr_node: *NodeU64 = @fieldParentPtr("node", n);
            allocator.destroy(curr_node);
        }
    }
    node.data = 1;
    list.append(&node.node);

    var prev_thickness: u64 = 1;
    var i: u64 = 2;

    while (true) : (i += 1) {
        var total_blocks: u64 = 0;
        const next_thickness = (prev_thickness * num_priests) % num_acolytes + num_acolytes;

        var curr = list.first;
        while (curr) |curr_p| {
            const curr_node: *NodeU64 = @fieldParentPtr("node", curr_p);
            curr_node.data += next_thickness;

            curr = curr_p.next;
            total_blocks += curr_node.data;
        }

        var right_node = try allocator.create(NodeU64);
        var left_node = try allocator.create(NodeU64);
        // defer allocator.destroy(left_node);
        // defer allocator.destroy(right_node);

        left_node.data = next_thickness;
        right_node.data = next_thickness;
        prev_thickness = next_thickness;

        list.prepend(&left_node.node);
        list.append(&right_node.node);

        total_blocks += next_thickness + next_thickness;

        // do a removal check now though this is probably wasteful
        curr = list.first;
        var can_remove: u64 = 0;
        while (curr) |curr_p| {
            curr = curr_p.next;
            const curr_node: *NodeU64 = @fieldParentPtr("node", curr_p);
            if (curr_p == list.first or curr_p == list.last) {
                continue;
            }

            can_remove += (num_priests * (2 * i - 1) * curr_node.data) % num_acolytes;
        }

        if ((total_blocks - can_remove) >= num_blocks) {
            return total_blocks - can_remove - num_blocks;
        }
    }

    return 0;
}

test "given example (part 3)" {
    try expectEqual(2, try answer3(std.testing.allocator, 160, 5, 2));
    try expectEqual(0, try answer3(std.testing.allocator, 162, 5, 2));
    try expectEqual(0, try answer3(std.testing.allocator, 239, 5, 2));
    try expectEqual(0, try answer3(std.testing.allocator, 1885, 5, 2));
    try expectEqual(0, try answer3(std.testing.allocator, 1964801, 5, 2));
    try expectEqual(0, try answer3(std.testing.allocator, 125820925, 5, 2));
}
