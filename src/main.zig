const std = @import("std");
const quest1 = @import("quest1.zig");
const quest2 = @import("quest2.zig");
const quest3 = @import("quest3.zig");
const quest4 = @import("quest4.zig");
const quest5 = @import("quest5.zig");
const quest6 = @import("quest6.zig");
const quest7 = @import("quest7.zig");
const quest8 = @import("quest8.zig");
const quest9 = @import("quest9.zig");
const quest10 = @import("quest10.zig");
const quest11 = @import("quest11.zig");
const quest12 = @import("quest12.zig");
const quest13 = @import("quest13.zig");
const quest14 = @import("quest14.zig");
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

    // if (std.mem.eql(u8, quest, "quest2")) {
    //     const words_line = splits.first();
    //     var words_iterator = std.mem.splitScalar(u8, words_line, ':');
    //     _ = words_iterator.next();
    //     const words = words_iterator.next().?;
    //     _ = splits.next();

    //     if (std.mem.eql(u8, part, "1")) {
    //         const line = splits.next().?;
    //         std.debug.print("{d}\n", .{quest2.answer1(words, line)});
    //     }
    //     if (std.mem.eql(u8, part, "2")) {
    //         std.debug.print("{d}\n", .{quest2.answer2(words, splits)});
    //     }
    //     if (std.mem.eql(u8, part, "3")) {
    //         std.debug.print("{d}\n", .{quest2.answer3(words, lines[util.nthIndexOfScalar(u8, lines, '\n', 1).? + 1 ..])});
    //     }
    // }

    if (std.mem.eql(u8, quest, "quest3")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest3.answer1(allocator, lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest3.answer3(allocator, lines)});
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
            std.debug.print("{s}\n", .{try quest6.answer(allocator, lines, false)});
        } else {
            std.debug.print("{s}\n", .{try quest6.answer(allocator, lines, true)});
        }
    }
    if (std.mem.eql(u8, quest, "quest7")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{s}\n", .{try quest7.answer1(allocator, lines, 10)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{s}\n", .{try quest7.answer2(allocator, lines, 10)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest7.answer3(allocator, lines)});
        }
    }
    if (std.mem.eql(u8, quest, "quest8")) {
        const newline = std.mem.indexOfScalar(u8, lines, '\n');
        const num: u64 = try std.fmt.parseInt(u64, if (newline == null) lines else lines[0..newline.?], 10);
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{quest8.answer1(num)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{quest8.answer2(20240000, 1111, num)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest8.answer3(allocator, 202400000, 10, num)});
        }
    }
    if (std.mem.eql(u8, quest, "quest9")) {
        var it = util.NumberIterator{ .lines = lines };
        var mem = try allocator.alloc(u32, std.mem.count(u8, lines, "\n"));
        defer allocator.free(mem);
        var i: u32 = 0;
        while (it.next()) |n| : (i += 1) {
            mem[i] = n;
        }
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest9.answer1(allocator, mem, &[_]u32{ 1, 3, 5, 10 })});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{try quest9.answer2(allocator, mem, &[_]u32{ 1, 3, 5, 10, 15, 16, 20, 24, 25, 30 })});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest9.answer3(allocator, mem, &[_]u32{ 1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101 })});
        }
    }
    if (std.mem.eql(u8, quest, "quest10")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{s}\n", .{try quest10.answer1(allocator, lines)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{try quest10.answer2(allocator, lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest10.answer3(allocator, lines)});
        }
    }
    if (std.mem.eql(u8, quest, "quest11")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest11.answer1(allocator, lines, 4)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{try quest11.answer2(allocator, lines, 10)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest11.answer3(allocator, lines, 20)});
        }
    }
    if (std.mem.eql(u8, quest, "quest12")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest12.answer1(allocator, lines)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{try quest12.answer1(allocator, lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest12.answer3(allocator, lines)});
        }
    }
    if (std.mem.eql(u8, quest, "quest13")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest13.answer1(allocator, lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest13.answer3(allocator, lines)});
        }
    }
    if (std.mem.eql(u8, quest, "quest14")) {
        if (std.mem.eql(u8, part, "1")) {
            std.debug.print("{d}\n", .{try quest14.answer1(lines)});
        }
        if (std.mem.eql(u8, part, "2")) {
            std.debug.print("{d}\n", .{try quest14.answer2(allocator, lines)});
        }
        if (std.mem.eql(u8, part, "3")) {
            std.debug.print("{d}\n", .{try quest14.answer3(allocator, lines)});
        }
    }
}
