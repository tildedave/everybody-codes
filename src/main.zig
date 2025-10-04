const std = @import("std");
const quest1 = @import("quest1.zig");
const quest2 = @import("quest2.zig");
const quest3 = @import("quest3.zig");
const util = @import("util.zig");

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
        const line = splits.first();
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}", .{quest1.answer1(line)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}", .{quest1.answer2(line)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}", .{quest1.answer3(line)});
        }
    }

    if (std.mem.eql(u8, quest, "quest2")) {
        const words_line = splits.first();
        var words_iterator = std.mem.splitScalar(u8, words_line, ':');
        _ = words_iterator.next();
        const words = words_iterator.next().?;
        _ = splits.next();

        if (std.mem.eql(u8, part, "1")) {
            const line = splits.next().?;
            std.debug.print("{d}", .{quest2.answer1(words, line)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}", .{quest2.answer2(words, splits)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}", .{quest2.answer3(words, lines[util.nthIndexOfScalar(u8, lines, '\n', 1).? + 1 ..])});
        }
    }

    if (std.mem.eql(u8, quest, "quest3")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}", .{try quest3.answer1(lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}", .{try quest3.answer3(lines)});
        }
    }
}
