const std = @import("std");

fn getCalibrationValue(line: []const u8) u32 {
    var head: ?u32 = null;
    var last: ?u32 = null;

    for (line) |c| {
        var digit: ?u32 = null;
        if (std.ascii.isDigit(c)) {
            digit = c - '0';
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
