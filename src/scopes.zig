const std = @import("std");
const Allocator = std.mem.Allocator;

const Constant = @import("constants.zig");

pub const Node = struct {
    parent: ?Index = null,
    depth: usize,

    children: std.ArrayList(Index),
    symbols: std.StringHashMap(void),

    pub fn make(allocator: Allocator, depth: usize) Node {
        return Node{
            .parent = null,
            .depth = depth,
            .children = .init(allocator),
            .symbols = .init(allocator),
        };
    }

    pub fn makeWithParent(allocator: Allocator, depth: usize, parent: Index) Node {
        return Node{
            .parent = parent,
            .depth = depth,
            .children = .init(allocator),
            .symbols = .init(allocator),
        };
    }

    pub fn register(this: *Node, name: []const u8) void {
        this.symbols.put(name, void) catch |err| {
            @panic(@errorName(err));
        };
    }

    pub fn has(this: *Node, name: []const u8) bool {
        return this.symbols.get(name) == null;
    }

    pub fn deinit(this: *Node) void {
        this.parent = null;
        this.children.deinit();
        this.symbols.deinit();
    }
};

pub const Index = enum(usize) { _ };

allocator: Allocator,
list: std.ArrayList(Node),
current: Index = @enumFromInt(0),
depth: usize = 0,

const Self = @This();

pub fn make(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .list = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.list.deinit();
}

pub fn get_scope(self: *Self, idx: Index) *Node {
    return &self.scopes.items[@intFromEnum(idx)];
}

pub fn get(self: *Self) *Node {
    return self.get_scope(self.current);
}

pub fn down(self: *Self) void {
    const current = self.get_scope(self.current_scope);

    self.depth += 1;

    const child = Node.makeWithParent(self.allocator, self.depth, self.current_scope);
    const child_idx = self.scopes.items.len;

    self.scopes.append(child) catch |err| {
        @panic(@errorName(err));
    };

    current.children.append(child_idx) catch |err| {
        @panic(@errorName(err));
    };

    self.current_scope = child_idx;
}

pub fn up(self: *Self) void {
    const current = self.get_scope(self.current_scope);

    if (current.parent) |parent| {
        self.depth -= 1;
        self.current_scope = parent;
    }
}
