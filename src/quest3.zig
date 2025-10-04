const util = @import("util.zig");

test "can remove" {
    const lines: []u8 = "..........\n..###.##..\n...####...\n..######..\n..######..\n...####...\n..........\n";
    const grid = util.createGrid(lines);

    for (0..grid.lines.len) |idx| {
        if (grid.lines[idx] == '\n') {
            continue;
        }
    }
}
