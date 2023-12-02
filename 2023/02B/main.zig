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
        _ = header_it.next().?;
        const tail = header_it.next().?;

        var num_r: u32 = 0;
        var num_g: u32 = 0;
        var num_b: u32 = 0;

        var hand_it = std.mem.split(u8, tail, "; ");

        while (hand_it.next()) |hand| {
            var cube_it = std.mem.split(u8, hand, ", ");

            while (cube_it.next()) |cube| {
                var word_it = std.mem.split(u8, cube, " ");

                const num_str = word_it.next().?;
                const num = try std.fmt.parseInt(u32, num_str, 10);

                const colour_str = word_it.next().?;
                const colour = std.meta.stringToEnum(Colour, colour_str).?;
                switch (colour) {
                    .red => {
                        num_r = @max(num_r, num);
                    },
                    .green => {
                        num_g = @max(num_g, num);
                    },
                    .blue => {
                        num_b = @max(num_b, num);
                    },
                }
            }
        }

        const power = num_r * num_g * num_b;
        total += power;
    }

    try stdout.writer().print("{d}\n", .{total});
    try stdout.flush();
}
