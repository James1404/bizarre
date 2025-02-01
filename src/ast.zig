const std = @import("std");
const Token = @import("token.zig");

pub const NodeRef = *Node;
pub const Node = union(enum) {
    Error: struct {
        msg: []const u8,
        token: Token,
    },

    Binary: struct {
        lhs: NodeRef,
        rhs: NodeRef,
        Op: Token,
    },
    Unary: struct { Op: Token, node: NodeRef },

    Float: []const u8,
    Int: []const u8,
    String: []const u8,
    Bool: bool,

    Ident: Token,

    TopLevelScope: std.ArrayList(NodeRef),
    Scope: std.ArrayList(NodeRef),

    ConstDecl: struct { ident: NodeRef, type: ?NodeRef, value: NodeRef },
    VarDecl: struct { ident: NodeRef, type: ?NodeRef, value: NodeRef },

    Assignment: struct { ident: NodeRef, value: NodeRef },

    ParamaterList: std.ArrayList(NodeRef),
    Paramater: struct { ident: NodeRef, type: NodeRef },

    FnDecl: struct {
        params: NodeRef,
        ret: NodeRef,
        block: NodeRef,
    },
    FnCall: struct { @"fn": NodeRef, args: std.ArrayList(NodeRef) },

    Return: NodeRef,
    ImplicitReturn: NodeRef,

    Dot: struct { lhs: NodeRef, ident: NodeRef },

    If: struct { cond: NodeRef, true: NodeRef, false: NodeRef },

    Match: struct { value: NodeRef, branches: std.ArrayList(NodeRef) },
    MatchBranch: struct { pattern: NodeRef, value: NodeRef },

    Comptime: NodeRef,
    Extern: NodeRef,

    FnType: struct {
        params: NodeRef,
        ret: NodeRef,
    },

    Field: struct {
        ident: NodeRef,
        type: ?NodeRef,
        default: ?NodeRef,
    },

    Interface: struct { ident: NodeRef, fields: std.ArrayList(NodeRef) },
    Struct: struct { fields: std.ArrayList(NodeRef) },
    Distinct: NodeRef,
    PtrType: NodeRef,

    Defer: NodeRef,
    Cast: struct { value: NodeRef, type: NodeRef },
};

allocator: std.mem.Allocator,

root: ?NodeRef,

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
    for (0..indent) |_| out("  ", .{});

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

        .TopLevelScope => |lst| for (lst.items) |n| self.printNode(n, indent),
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

        .Assignment => |v| {
            self.printNode(v.ident, indent);
            self.printNode(v.value, indent);
        },

        .ParamaterList => |lst| for (lst.items) |n| self.printNode(n, indent),
        .Paramater => |v| {
            self.printNode(v.ident, indent);
            self.printNode(v.type, indent);
        },

        .FnDecl => |v| {
            self.printNode(v.params, indent);
            self.printNode(v.ret, indent);
            self.printNode(v.block, indent);
        },
        .FnCall => |v| {
            self.printNode(v.@"fn", indent);
            for (v.args.items) |n| self.printNode(n, indent);
        },

        .Return => |n| self.printNode(n, indent),
        .ImplicitReturn => |n| self.printNode(n, indent),

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

        .Comptime => |n| self.printNode(n, indent),
        .Extern => |n| self.printNode(n, indent),

        .FnType => |v| {
            self.printNode(v.ret, indent);
            self.printNode(v.params, indent);
        },

        .Field => |v| {
            self.printNode(v.ident, indent);
            if (v.type) |n| self.printNode(n, indent);
            if (v.default) |n| self.printNode(n, indent);
        },
        .Interface => |v| {
            self.printNode(v.ident, indent);
            for (v.fields.items) |n| self.printNode(n, indent);
        },
        .Struct => |v| for (v.fields.items) |n| self.printNode(n, indent),
        .Distinct => |n| self.printNode(n, indent),
        .PtrType => |n| self.printNode(n, indent),

        .Defer => |n| self.printNode(n, indent),
        .Cast => |v| {
            self.printNode(v.value, indent);
            self.printNode(v.type, indent);
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

        .TopLevelScope => |lst| {
            for (lst.items) |n| self.freeNode(n);
            lst.deinit();
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

        .Assignment => |v| {
            self.freeNode(v.ident);
            self.freeNode(v.value);
        },

        .ParamaterList => |lst| {
            for (lst.items) |n| self.freeNode(n);
            lst.deinit();
        },
        .Paramater => |v| {
            self.freeNode(v.ident);
            self.freeNode(v.type);
        },

        .FnDecl => |v| {
            self.freeNode(v.params);
            self.freeNode(v.ret);
            self.freeNode(v.block);
        },
        .FnCall => |v| {
            for (v.args.items) |n| self.freeNode(n);
            v.args.deinit();

            self.freeNode(v.@"fn");
        },

        .Return => |n| self.freeNode(n),
        .ImplicitReturn => |n| self.freeNode(n),

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

        .Comptime => |n| self.freeNode(n),
        .Extern => |n| self.freeNode(n),

        .FnType => |v| {
            self.freeNode(v.params);
            self.freeNode(v.ret);
        },

        .Field => |v| {
            self.freeNode(v.ident);
            if (v.type) |n| self.freeNode(n);
            if (v.default) |n| self.freeNode(n);
        },
        .Interface => |v| {
            self.freeNode(v.ident);
            for (v.fields.items) |n| self.freeNode(n);
            v.fields.deinit();
        },
        .Struct => |v| {
            for (v.fields.items) |n| self.freeNode(n);
            v.fields.deinit();
        },
        .Distinct => |n| self.freeNode(n),
        .PtrType => |n| self.freeNode(n),

        .Defer => |n| self.freeNode(n),

        .Cast => |v| {
            self.freeNode(v.value);
            self.freeNode(v.type);
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
