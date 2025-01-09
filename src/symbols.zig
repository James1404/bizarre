const std = @import("std");

const ScopeRef = *Scope;
const Scope = struct {
    parent: ?ScopeRef,
    children: std.ArrayList(ScopeRef),

    symbols: std.StringHashMap(void),
};

allocator: std.mem.Allocator,
root: ?ScopeRef,
current: ?ScopeRef,

const Self = @This();

pub fn make(allocator: std.mem.Allocator) Self {
    const self = Self{
        .allocator = allocator,
        .root = null,
        .current = null,
    };

    self.root = self.newScope();

    return self;
}

pub fn deinit(self: *Self) void {
    self.freeScope(self.root);
}

pub fn newScope(self: *Self) void {
    if (self.current) |current| {
        current.children.append(
            self.newScope(),
        );
    }
}

fn freeScope(self: *Self, scope: ScopeRef) void {
    for (scope.children) |child| self.freeScope(child);
    scope.children.deinit();

    scope.symbols.deinit();

    scope.parent = null;

    self.allocator.destroy(scope);
}

fn alloc(self: *Self) ScopeRef {
    const ptr = self.allocator.create(Scope) catch |err| {
        @panic(@errorName(err));
    };

    ptr.* = .{
        .children = .init(self.allocator),
        .parent = self.current orelse null,
        .symbols = .init(self.allocator),
    };

    if (self.current == null) {
        self.current = ptr;
    }

    return ptr;
}

pub fn up(self: *Self) void {
    self.current = if (self.current) |current| current.parent else null;
}
