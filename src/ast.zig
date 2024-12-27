const std = @import("std");
const Token = @import("token.zig");

pub const Node = union(enum) {
    Error: struct {
        msg: []const u8,
        token: Token,
    },

    Binary: struct {
        lhs: *Node,
        rhs: *Node,
        Op: Token,
    },
    Unary: struct { Op: Token, Value: *Node },

    Float: []const u8,
    Int: []const u8,
    String: []const u8,
    Bool: bool,

    Ident: Token,

    Scope: std.ArrayList(*Node),

    ConstDecl: struct { ident: *Node, type: ?*Node, value: *Node },
    VarDecl: struct { ident: *Node, type: ?*Node, value: *Node },

    Paramater: struct { ident: *Node, type: ?*Node },
    FnDecl: struct { params: std.ArrayList(*Node), Ret: ?*Node },
    FnCall: struct { @"fn": *Node, args: std.ArrayList(*Node) },

    Dot: struct { lhs: *Node, ident: *Node },

    If: struct { cond: *Node, true: *Node, false: *Node },

    Match: struct { value: *Node, branches: std.ArrayList(*Node) },
    MatchBranch: struct { pattern: *Node, value: *Node },
};

arena: std.heap.ArenaAllocator,
allocator: std.mem.Allocator,

root: ?*Node,

const Self = @This();

pub fn make(parent_allocator: std.mem.Allocator) Self {
    var result: Self = undefined;

    result.arena = .init(parent_allocator);
    result.allocator = result.arena.allocator();

    result.root = null;

    return result;
}

pub fn deinit(self: *Self) void {
    self.root = null;
    self.arena.deinit();
}

pub fn alloc(self: *Self, node: Node) *Node {
    const ptr = self.allocator.create(Node) catch |err| {
        @panic(@errorName(err));
    };

    ptr.* = node;

    return ptr;
}
