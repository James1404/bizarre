const std = @import("std");
const Token = @import("token.zig");

pub const NodeRef = *Node;
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
    Unary: struct { Op: Token, node: *Node },

    Float: []const u8,
    Int: []const u8,
    String: []const u8,
    Bool: bool,

    Ident: Token,

    Scope: std.ArrayList(*Node),

    ConstDecl: struct { ident: *Node, type: ?*Node, value: *Node },
    VarDecl: struct { ident: *Node, type: ?*Node, value: *Node },

    Paramater: struct { ident: *Node, type: *Node },
    FnDecl: struct {
        params: std.ArrayList(*Node),
        ret: *Node,
        block: *Node,
    },
    FnCall: struct { @"fn": *Node, args: std.ArrayList(*Node) },

    Dot: struct { lhs: *Node, ident: *Node },

    If: struct { cond: *Node, true: *Node, false: *Node },

    Match: struct { value: *Node, branches: std.ArrayList(*Node) },
    MatchBranch: struct { pattern: *Node, value: *Node },
};

allocator: std.mem.Allocator,

root: ?*Node,

const Self = @This();

pub fn make(allocator: std.mem.Allocator) Self {
    var result: Self = undefined;

    result.allocator = allocator;

    result.root = null;

    return result;
}

pub fn deinit(self: *Self) void {
    if (self.root) |root| {
        self.freeNode(root);
    }

    self.root = null;
}

pub fn print(self: *Self) void {
    if (self.root) |root| {
        self.printNode(root, 0);
    }
}

fn printIndent(indent: u32, comptime fmt: []const u8, args: anytype) void {
    const out = std.debug.print;
    for (0..indent) |_| out("    ", .{});

    out(fmt, args);
    out("\n", .{});
}

fn printNode(self: *Self, node: NodeRef, start_indent: u32) void {
    printIndent(start_indent, "{s}", .{@tagName(node.*)});

    const indent = start_indent + 1;

    switch (node.*) {
        .Error => |err| {
            printIndent(indent, "{s}", .{err.msg});
            printIndent(indent, "\"{s}\"", .{err.token.text});
        },

        .Binary => |v| {
            printIndent(indent, "\"{s}\"", .{v.Op.text});
            self.printNode(v.lhs, indent);
            self.printNode(v.rhs, indent);
        },
        .Unary => |v| {
            printIndent(indent, "\"{s}\"", .{v.Op.text});
            self.printNode(v.node, indent);
        },

        .Float => |v| printIndent(indent, "{s}", .{v}),
        .Int => |v| printIndent(indent, "{s}", .{v}),
        .String => |v| printIndent(indent, "\"{s}\"", .{v}),
        .Bool => |v| printIndent(indent, "{s}", .{if (v) "true" else "false"}),

        .Ident => |v| printIndent(indent, "{s}", .{v.text}),

        .Scope => |lst| for (lst.items) |n| self.printNode(n, indent),

        .ConstDecl => |v| {
            self.printNode(v.ident, indent);

            if (v.type) |ty| {
                self.printNode(ty, indent);
            }

            self.printNode(v.value, indent);
        },
        .VarDecl => |v| {
            self.printNode(v.ident, indent);

            if (v.type) |ty| {
                self.printNode(ty, indent);
            }

            self.printNode(v.value, indent);
        },

        .Paramater => |v| {
            self.printNode(v.ident, indent);
            self.printNode(v.type, indent);
        },
        .FnDecl => |v| {
            for (v.params.items) |n| self.printNode(n, indent);
            self.printNode(v.ret, indent);
            self.printNode(v.block, indent);
        },
        .FnCall => |v| {
            self.printNode(v.@"fn", indent);
            for (v.args.items) |n| self.printNode(n, indent);
        },

        .Dot => |v| {
            self.printNode(v.lhs, indent);
            self.printNode(v.ident, indent);
        },

        .If => |v| {
            self.printNode(v.cond, indent);
            self.printNode(v.true, indent);
            self.printNode(v.false, indent);
        },

        .Match => |v| {
            self.printNode(v.value, indent);
            for (v.branches.items) |n| self.printNode(n, indent);
        },
        .MatchBranch => |v| {
            self.printNode(v.pattern, indent);
            self.printNode(v.value, indent);
        },
    }
}

fn freeNode(self: *Self, node: NodeRef) void {
    switch (node.*) {
        .Binary => |v| {
            self.freeNode(v.lhs);
            self.freeNode(v.rhs);
        },
        .Unary => |v| {
            self.freeNode(v.node);
        },

        .Scope => |lst| {
            for (lst.items) |n| self.freeNode(n);
            lst.deinit();
        },

        .ConstDecl => |v| {
            self.freeNode(v.ident);
            if (v.type) |t| self.freeNode(t);
            self.freeNode(v.value);
        },
        .VarDecl => |v| {
            self.freeNode(v.ident);
            if (v.type) |t| self.freeNode(t);
            self.freeNode(v.value);
        },

        .Paramater => |v| {
            self.freeNode(v.ident);
            self.freeNode(v.type);
        },
        .FnDecl => |v| {
            for (v.params.items) |n| self.freeNode(n);
            v.params.deinit();

            self.freeNode(v.ret);
            self.freeNode(v.block);
        },
        .FnCall => |v| {
            for (v.args.items) |n| self.freeNode(n);
            v.args.deinit();

            self.freeNode(v.@"fn");
        },

        .Dot => |v| {
            self.freeNode(v.lhs);
            self.freeNode(v.ident);
        },

        .If => |v| {
            self.freeNode(v.cond);
            self.freeNode(v.true);
            self.freeNode(v.false);
        },

        .Match => |v| {
            self.freeNode(v.value);

            for (v.branches.items) |n| {
                self.freeNode(n);
            }
            v.branches.deinit();
        },
        .MatchBranch => |v| {
            self.freeNode(v.pattern);
            self.freeNode(v.value);
        },

        .Float,
        .Int,
        .String,
        .Bool,
        .Ident,
        .Error,
        => {},
    }

    self.allocator.destroy(node);
}

pub fn alloc(self: *Self, node: Node) NodeRef {
    const ptr = self.allocator.create(Node) catch |err| {
        @panic(@errorName(err));
    };

    ptr.* = node;

    return ptr;
}
