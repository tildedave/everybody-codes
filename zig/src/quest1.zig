const std = @import("std");
const expectEqual = std.testing.expectEqual;

fn bugScore(ch: u8) u8 {
    if (ch == 'A') {
        return 0;
    }
    if (ch == 'B') {
        return 1;
    }
    if (ch == 'C') {
        return 3;
    }
    if (ch == 'D') {
        return 5;
    }

    return 0;
}

pub fn answer1(line: []const u8) u32 {
    var result: u32 = 0;

    for (line) |ch| {
        result += bugScore(ch);
    }

    return result;
}

test "answer1 expected" {
    try expectEqual(answer1("ABBAC"), 5);
}

fn groupScore(group: []const u8) u8 {
    var num_bugs: u8 = 0;
    var total_score: u8 = 0;

    for (group) |ch| {
        if (ch != 'x') {
            num_bugs += 1;
        }
        total_score += bugScore(ch);
    }

    if (num_bugs == 2) {
        total_score += 2;
    }
    if (num_bugs == 3) {
        total_score += 6;
    }

    return total_score;
}

fn bugParse(line: []const u8, offset: u8) u32 {
    var i: u16 = 0;
    var result: u32 = 0;

    while (i < line.len) {
        result += groupScore(line[i .. i + offset]);
        i += offset;
    }

    return result;
}

pub fn answer2(line: []const u8) u32 {
    return bugParse(line, 2);
}

test "answer2 expected" {
    try expectEqual(answer2("AxBCDDCAxD"), 28);
}

pub fn answer3(line: []const u8) u32 {
    return bugParse(line, 3);
}

test "answer3 expected" {
    try expectEqual(answer3("xBxAAABCDxCC"), 30);
}
