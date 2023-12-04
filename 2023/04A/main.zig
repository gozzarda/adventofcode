const std = @import("std");

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

        var header_it = std.mem.split(u8, line.items, ":");
        _ = header_it.next().?;
        const body = header_it.next().?;

        var part_it = std.mem.split(u8, body, "|");
        const lhs = part_it.next().?;
        const rhs = part_it.next().?;

        var lword_it = std.mem.split(u8, lhs, " ");

        var lvals = std.ArrayList(u32).init(allocator);
        defer lvals.deinit();

        while (lword_it.next()) |lword| {
            if (lword.len > 0) {
                try lvals.append(try std.fmt.parseInt(u32, lword, 10));
            }
        }

        var rword_it = std.mem.split(u8, rhs, " ");

        var matches: u32 = 0;

        while (rword_it.next()) |rword| {
            if (rword.len > 0) {
                const rval: u32 = try std.fmt.parseInt(u32, rword, 10);

                for (lvals.items) |lval| {
                    if (lval == rval) {
                        matches += 1;
                        break;
                    }
                }
            }
        }

        if (matches > 0) {
            total += try std.math.powi(u32, 2, matches - 1);
        }
    }

    try stdout.writer().print("{d}\n", .{total});
    try stdout.flush();
}
