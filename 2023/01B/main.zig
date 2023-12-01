const std = @import("std");

fn isPrefixOf(lhs: []const u8, rhs: []const u8) bool {
    if (lhs.len > rhs.len) return false;
    for (lhs, rhs[0..lhs.len]) |l, r| {
        if (l != r) return false;
    }
    return true;
}

fn getCalibrationValue(line: []const u8) u32 {
    const words = [_][]const u8{
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
    };

    var head: ?u32 = null;
    var last: ?u32 = null;

    for (0.., line) |i, c| {
        var digit: ?u32 = null;
        if (std.ascii.isDigit(c)) {
            digit = c - '0';
        } else for (1.., words) |value, word| {
            if (isPrefixOf(word, line[i..])) {
                digit = @truncate(value);
                break;
            }
        }
        if (head == null) head = digit;
        if (digit != null) last = digit;
    }

    return head.? * 10 + last.?;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdin = std.io.bufferedReader(std.io.getStdIn().reader());
    var stdout = std.io.bufferedWriter(std.io.getStdOut().writer());

    var line = std.ArrayList(u8).init(allocator);
    defer line.deinit();

    var total: u32 = 0;

    var done = false;
    while (!done) {
        line.clearRetainingCapacity();
        stdin.reader().streamUntilDelimiter(line.writer(), '\n', null) catch |err| switch (err) {
            error.EndOfStream => done = true,
            else => return err,
        };
        total += getCalibrationValue(line.items);
    }

    try stdout.writer().print("{d}\n", .{total});
    try stdout.flush();
}
