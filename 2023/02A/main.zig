const std = @import("std");

const Colour = enum { red, green, blue };

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

        var header_it = std.mem.split(u8, line.items, ": ");
        const head = header_it.next().?;
        const tail = header_it.next().?;

        var id_it = std.mem.split(u8, head, " ");
        _ = id_it.next();
        const id_str = id_it.next().?;
        const id = try std.fmt.parseInt(u32, id_str, 10);

        var valid = true;

        var hand_it = std.mem.split(u8, tail, "; ");

        while (hand_it.next()) |hand| {
            var cube_it = std.mem.split(u8, hand, ", ");

            while (cube_it.next()) |cube| {
                var word_it = std.mem.split(u8, cube, " ");

                const num_str = word_it.next().?;
                const num = try std.fmt.parseInt(u32, num_str, 10);

                const colour_str = word_it.next().?;
                const colour = std.meta.stringToEnum(Colour, colour_str).?;
                const limit: u32 = switch (colour) {
                    .red => 12,
                    .green => 13,
                    .blue => 14,
                };

                if (num > limit) valid = false;
                if (!valid) break;
            }

            if (!valid) break;
        }

        if (valid) total += id;
    }

    try stdout.writer().print("{d}\n", .{total});
    try stdout.flush();
}
