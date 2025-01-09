const std = @import("std");
const Allocator = std.mem.Allocator;

const BlockList = @import("blocklist.zig");
const Repr = @import("repr.zig");

const Fn = struct {
    const Param = struct {
        ty: Repr.Ref,
        name: []const u8,
    };

    params: std.ArrayList(Param),
    body: BlockList.Location,
};

allocator: Allocator,
inner: std.ArrayList(Fn),

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .inner = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.inner.deinit();
}
