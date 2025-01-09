const std = @import("std");
const Allocator = std.mem.Allocator;

const Repr = @import("repr.zig");
const Inst = Repr.Inst;

pub const Location = usize;
pub const Block = struct {
    instructions: std.ArrayList(Inst),

    parent: ?Location = null,
    children: std.ArrayList(Location),
    sealed: bool = false,
    temp_counter: usize = 0,

    const Self = @This();

    pub fn make(allocator: Allocator) @This() {
        return @This(){
            .instructions = .init(allocator),
            .children = .init(allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.instructions.deinit();
        self.children.deinit();
    }

    pub fn append(self: *@This(), inst: Inst) void {
        self.instructions.append(inst) catch |err| {
            @panic(@errorName(err));
        };
    }
};

allocator: Allocator,
data: std.ArrayList(Block),

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .data = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.data.deinit();
}

pub fn get(self: *Self, loc: Location) *Block {
    return &self.data.items[loc];
}

pub fn append(self: *Self, block: Block) Location {
    const idx = self.data.items.len;
    self.data.append(block) catch |err| {
        @panic(@errorName(err));
    };
    return idx;
}
