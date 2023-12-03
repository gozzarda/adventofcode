const std = @import("std");

const Prod = struct {
    const Self = @This();

    prod: u32,
    terms: u32,

    pub fn init() Self {
        return Self{
            .prod = 1,
            .terms = 0,
        };
    }

    pub fn mult(self: *Self, term: u32) void {
        self.prod *= term;
        self.terms += 1;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var stdin = std.io.bufferedReader(std.io.getStdIn().reader());
    var stdout = std.io.bufferedWriter(std.io.getStdOut().writer());

    var grid = std.ArrayList(std.ArrayList(u8)).init(allocator);
    defer grid.deinit();
    defer for (grid.items) |line| {
        line.deinit();
    };

    var prods = std.ArrayList(std.ArrayList(Prod)).init(allocator);
    defer prods.deinit();
    defer for (prods.items) |line| {
        line.deinit();
    };

    var lines_done = false;
    while (!lines_done) {
        try grid.append(std.ArrayList(u8).init(allocator));
        stdin.reader().streamUntilDelimiter(grid.items[grid.items.len - 1].writer(), '\n', null) catch |err| switch (err) {
            error.EndOfStream => lines_done = true,
            else => return err,
        };

        const len = grid.items[grid.items.len - 1].items.len;
        try prods.append(std.ArrayList(Prod).init(allocator));
        try prods.items[prods.items.len - 1].appendNTimes(Prod.init(), len);
    }

    for (0.., grid.items) |row, line| {
        const row_lwr = @max(1, row) - 1;
        const row_upr = @min(row + 2, grid.items.len);
        var col: usize = 0;
        while (col < line.items.len) {
            const lwr = col;
            while (col < line.items.len and std.ascii.isDigit(line.items[col])) {
                col += 1;
            }
            const upr = col;
            if (lwr == upr) {
                col += 1;
                continue;
            }
            const val = try std.fmt.parseInt(u32, line.items[lwr..upr], 10);
            const col_lwr = @max(1, lwr) - 1;
            const col_upr = @min(upr + 1, line.items.len);
            for (row_lwr..row_upr) |r| {
                for (col_lwr..col_upr) |c| {
                    if (grid.items[r].items[c] != '.' and !std.ascii.isDigit(grid.items[r].items[c])) {
                        prods.items[r].items[c].mult(val);
                    }
                }
            }
        }
    }

    var total: u32 = 0;

    for (prods.items) |row| {
        for (row.items) |prod| {
            if (prod.terms == 2) {
                total += prod.prod;
            }
        }
    }

    try stdout.writer().print("{d}\n", .{total});
    try stdout.flush();
}
