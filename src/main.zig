const std = @import("std");
const quest1 = @import("quest1.zig");
const quest2 = @import("quest2.zig");
const quest3 = @import("quest3.zig");
const quest4 = @import("quest4.zig");
const quest5 = @import("quest5.zig");
const quest6 = @import("quest6.zig");
const util = @import("util.zig");

pub fn main() !void {
    var iter = std.process.args();
    _ = iter.next();

    const quest = iter.next().?;
    const part = iter.next().?;
    const input_file_name = iter.next().?;
    var input_file = try std.fs.cwd().openFile(input_file_name, .{});
    const allocator = std.heap.page_allocator;
    defer input_file.close();
    const lines = try input_file.readToEndAlloc(allocator, 2_000_000);
    defer allocator.free(lines);

    var splits = std.mem.splitScalar(u8, lines, '\n');

    if (std.mem.eql(u8, quest, "quest1")) {
        const line = splits.first();
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{quest1.answer1(line)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{quest1.answer2(line)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{quest1.answer3(line)});
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
            std.debug.print("{d}\n", .{quest2.answer1(words, line)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{quest2.answer2(words, splits)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{quest2.answer3(words, lines[util.nthIndexOfScalar(u8, lines, '\n', 1).? + 1 ..])});
        }
    }

    if (std.mem.eql(u8, quest, "quest3")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest3.answer1(lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest3.answer3(lines)});
        }
    }
    if (std.mem.eql(u8, quest, "quest4")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{quest4.answer1(lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{quest4.answer3(lines)});
        }
    }
    if (std.mem.eql(u8, quest, "quest5")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest5.answer1(lines, 10, allocator)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{try quest5.answer2(lines, allocator)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest5.answer3(lines, allocator)});
        }
    }
    if (std.mem.eql(u8, quest, "quest6")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{s}\n", .{try quest6.answer1(allocator, lines)});
        }
    }
}
