const std = @import("std");

pub fn main() !void {
    var stdin_buffer: [512]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin: *std.Io.Reader = &stdin_reader.interface;

    var stdout_buffer: [512]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_writer.interface;

    const dial_size: u32 = 100;
    var dial_pos: u32 = 50;
    var num_zeros: u32 = 0;

    while (try stdin.takeDelimiter('\n')) |line| {
        const dist: u32 = try std.fmt.parseInt(u32, line[1..], 10);

        const offset: u32 = switch (line[0]) {
            'L' => @mod(dial_size - dial_pos, dial_size),
            'R' => @mod(dial_size + dial_pos, dial_size),
            else => unreachable,
        };

        num_zeros += (dist + offset) / dial_size;

        const dist_mod: u32 = @mod(dist, dial_size);

        dial_pos = @mod(switch (line[0]) {
            'L' => @mod(dial_size + dial_pos - dist_mod, dial_size),
            'R' => @mod(dial_size + dial_pos + dist_mod, dial_size),
            else => unreachable,
        }, dial_size);
    }

    try stdout.print("{d}\n", .{num_zeros});
    try stdout.flush();
}
