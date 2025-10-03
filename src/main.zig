const std = @import("std");
const quest1 = @import("quest1.zig");

pub fn main() !void {
    var iter = std.process.args();
    _ = iter.next();

    const quest = iter.next().?;
    const part = iter.next().?;
    const input_file_name = iter.next().?;
    var input_file = try std.fs.cwd().openFile(input_file_name, .{});
    defer input_file.close();
    const lines = try input_file.readToEndAlloc(std.heap.page_allocator, 2_000_000);
    var splits = std.mem.splitScalar(u8, lines, '\n');

    if (std.mem.eql(u8, quest, "quest1")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}", .{quest1.answer1(splits.first())});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}", .{quest1.answer2(splits.first())});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}", .{quest1.answer3(splits.first())});
        }
    }
}
