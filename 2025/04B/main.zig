const std = @import("std");
const Allocator = std.mem.Allocator;

const Grid = struct {
    const Self = @This();

    data: std.ArrayList(u8),
    num_rows: usize,
    num_cols: usize,

    pub const empty: Self = .{
        .data = .empty,
        .num_rows = 0,
        .num_cols = 0,
    };

    pub fn init(alloc: Allocator, num_rows: usize, num_cols: usize) Allocator.Error!Self {
        var data: std.ArrayList(u8) = .empty;
        try data.resize(alloc, num_rows * num_cols);
        return .{
            .data = data,
            .num_rows = num_rows,
            .num_cols = num_cols,
        };
    }

    pub fn deinit(self: *Self, alloc: Allocator) void {
        self.data.deinit(alloc);
    }

    pub fn appendRow(self: *Self, alloc: Allocator, row: []u8) Allocator.Error!void {
        if (self.data.items.len == 0) self.num_cols = row.len;
        std.debug.assert(row.len == self.num_cols);
        try self.data.appendSlice(alloc, row);
        self.num_rows += 1;
        std.debug.assert(self.num_rows * self.num_cols == self.data.items.len);
    }

    pub fn get(self: *Self, r: usize, c: usize) u8 {
        return self.data.items[r * self.num_cols + c];
    }

    pub fn set(self: *Self, r: usize, c: usize, x: u8) void {
        self.data.items[r * self.num_cols + c] = x;
    }

    pub fn inc(self: *Self, r: usize, c: usize) u8 {
        self.data.items[r * self.num_cols + c] += 1;
        return self.data.items[r * self.num_cols + c];
    }

    pub fn dec(self: *Self, r: usize, c: usize) u8 {
        self.data.items[r * self.num_cols + c] -= 1;
        return self.data.items[r * self.num_cols + c];
    }
};

const Z2 = struct { isize, isize };

const deltas: [8]Z2 = .{
    .{ -1, -1 }, .{ -1, 0 }, .{ -1, 1 }, .{ 0, -1 },
    .{ 0, 1 },   .{ 1, -1 }, .{ 1, 0 },  .{ 1, 1 },
};

pub fn main() !void {
    // Set up allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // Buffered reader from stdin
    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().readerStreaming(&stdin_buffer);
    const stdin: *std.Io.Reader = &stdin_reader.interface;

    // Allocating writer to accumulate lines from stdin
    var line_writer = std.Io.Writer.Allocating.init(alloc);
    defer line_writer.deinit();

    // Grid of cell values
    var grid: Grid = .empty;
    defer grid.deinit(alloc);

    // Consume lines from input as rows of grid
    while (stdin.streamDelimiter(&line_writer.writer, '\n')) |_| {
        stdin.toss(1); // Discard delimiter
        try grid.appendRow(alloc, line_writer.written());
        line_writer.clearRetainingCapacity();
    } else |err| if (err != error.EndOfStream) return err;

    // Grid of adjacent neighbour counts
    var adjs: Grid = try .init(alloc, grid.num_rows, grid.num_cols);
    defer adjs.deinit(alloc);

    for (0..adjs.num_rows) |r| {
        for (0..adjs.num_cols) |c| {
            adjs.set(r, c, 0);
        }
    }

    for (0..adjs.num_rows) |r| {
        for (0..adjs.num_cols) |c| {
            if (grid.get(r, c) == '@') {
                for (deltas) |d| {
                    const dr, const dc = d;
                    const nr = @as(isize, @intCast(r)) + dr;
                    const nc = @as(isize, @intCast(c)) + dc;
                    const validRow = 0 <= nr and nr < adjs.num_rows;
                    const validCol = 0 <= nc and nc < adjs.num_cols;
                    if (validRow and validCol) {
                        _ = adjs.inc(@intCast(nr), @intCast(nc));
                    }
                }
            }
        }
    }

    // Stack (ArrayList) of removable tiles
    var stack: std.ArrayList(Z2) = .empty;
    defer stack.deinit(alloc);

    for (0..grid.num_rows) |r| {
        for (0..grid.num_cols) |c| {
            if (grid.get(r, c) == '@' and adjs.get(r, c) < 4) {
                try stack.append(alloc, .{ @intCast(r), @intCast(c) });
            }
        }
    }

    // Greedily remove tiles and update adjs and stack
    var removed: usize = 0;
    while (stack.pop()) |rc| {
        removed += 1;
        const r, const c = rc;
        for (deltas) |d| {
            const dr, const dc = d;
            const nr = r + dr;
            const nc = c + dc;
            const validRow = 0 <= nr and nr < adjs.num_rows;
            const validCol = 0 <= nc and nc < adjs.num_cols;
            if (validRow and validCol) {
                if (adjs.dec(@intCast(nr), @intCast(nc)) == 3 and
                    grid.get(@intCast(nr), @intCast(nc)) == '@')
                {
                    try stack.append(alloc, .{ nr, nc });
                }
            }
        }
    }

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_writer.interface;

    try stdout.print("{d}\n", .{removed});
    try stdout.flush();
}
