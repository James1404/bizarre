const std = @import("std");
const Repr = @import("repr.zig");
const Allocator = std.mem.Allocator;
const BlockList = @import("blocklist.zig");

allocator: Allocator,
blocks: BlockList,

const Self = @This();

pub fn deinit(self: *Self) void {
    self.blocks.deinit();
}
