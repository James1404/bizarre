const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("ast.zig");

const Scopes = @import("scopes.zig");

pub const Inst = union(enum) {
    add: struct { lhs: Ref, rhs: Ref },
    sub: struct { lhs: Ref, rhs: Ref },
    div: struct { lhs: Ref, rhs: Ref },
    mul: struct { lhs: Ref, rhs: Ref },

    cmp_lt: struct { lhs: Ref, rhs: Ref },
    cmp_gt: struct { lhs: Ref, rhs: Ref },
    cmp_le: struct { lhs: Ref, rhs: Ref },
    cmp_ge: struct { lhs: Ref, rhs: Ref },
    cmp_eq: struct { lhs: Ref, rhs: Ref },
    cmp_ne: struct { lhs: Ref, rhs: Ref },

    negate: struct { value: Ref },

    cast: struct { value: Ref, type: Ref },

    assign: struct { value: Ref },

    load: struct { identifier: []const u8 },
    set: struct { identifier: []const u8, value: Ref },

    int: []const u8,
    float: []const u8,
    string: []const u8,
    bool: bool,
    fn_ref: FnIndex,

    push_param: Ref,
    pop_param: struct { addr: Ref },

    call: Ref,
};

pub const FnIndex = enum(usize) { _ };
pub const Fn = struct {
    const Param = struct {
        ty: Ref,
        name: []const u8,
    };

    params: std.ArrayList(Param),
    ret: Ref,
    start: Location,
};

pub const Ref = enum(usize) { _ };

pub const Terminator = union(enum) {
    @"if": struct { cond: Ref, true: Location, false: Location },
    @"switch": struct {
        value: Ref,
        patterns: std.ArrayList(Ref),
    },
    goto: Location,
    @"return",
};

pub const Location = enum(usize) { _ };

pub const Block = struct {
    instructions: std.ArrayList(Inst),
    terminator: ?Terminator = null,
    counter: usize = 1,

    parent: ?Location = null,

    const Self = @This();

    pub fn make(allocator: Allocator) @This() {
        return @This(){
            .instructions = .init(allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.instructions.deinit();
        self.terminator = null;
    }

    pub fn append(self: *@This(), inst: Inst) Ref {
        const idx = self.instructions.items.len;
        self.instructions.append(inst) catch |err| {
            @panic(@errorName(err));
        };
        return @enumFromInt(idx);
    }
};

allocator: Allocator,
functions: std.ArrayList(Fn),
blocks: std.ArrayList(Block),
scopes: Scopes,
global_block: Location,
ast: AST,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    var self = Self{
        .allocator = allocator,
        .functions = .init(allocator),
        .blocks = .init(allocator),
        .scopes = .make(allocator),
        .global_block = undefined,
        .ast = ast,
    };

    self.global_block = self.new_block();
    return self;
}

pub fn deinit(self: *Self) void {
    self.functions.deinit();
    self.blocks.deinit();
    self.scopes.deinit();
}

pub fn new_block(self: *Self) Location {
    const idx = self.blocks.items.len;

    self.blocks.append(Block.make(self.allocator)) catch |err| {
        @panic(@errorName(err));
    };

    return @enumFromInt(idx);
}

pub fn get(self: *Self, loc: Location) *Block {
    return &self.blocks.items[@intFromEnum(loc)];
}

fn emit_fn(self: *Self, node: AST.NodeRef) FnIndex {
    const @"fn": Fn = switch (node.*) {
        .FnDecl => |_| node: {
            break :node undefined;
        },
        else => undefined,
    };

    const idx = self.functions.items.len;
    self.functions.append(@"fn") catch |err| {
        @panic(@errorName(err));
    };
    return @enumFromInt(idx);
}

fn emitExpression(
    self: *Self,
    loc: Location,
    node: AST.NodeRef,
) Ref {
    const block = self.get(loc);

    return switch (node.*) {
        .Unary => |n| node: {
            const value = self.emitExpression(loc, n.node);
            break :node switch (n.Op.ty) {
                .Minus => block.append(.{ .negate = .{
                    .value = value,
                } }),
                else => unreachable,
            };
        },
        .Binary => |n| node: {
            const lhs = self.emitExpression(loc, n.lhs);
            const rhs = self.emitExpression(loc, n.rhs);

            break :node switch (n.Op.ty) {
                .Plus => block.append(.{ .add = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Minus => block.append(.{ .sub = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Asterix => block.append(.{ .mul = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Slash => block.append(.{ .div = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                .Less => block.append(.{ .cmp_lt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .LessEq => block.append(.{ .cmp_le = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Greater => block.append(.{ .cmp_gt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .GreaterEq => block.append(.{ .cmp_ge = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                else => unreachable,
            };
        },
        .String => |v| block.append(.{ .string = v }),
        .Int => |v| block.append(.{ .int = v }),
        .Float => |v| block.append(.{ .float = v }),
        .Bool => |v| block.append(.{ .bool = v }),
        .Ident => |v| node: {
            break :node block.append(.{ .load = .{
                .identifier = v.text,
            } });
        },
        .FnCall => |n| node: {
            for (n.args.items) |arg| {
                const ref = self.emitExpression(loc, arg);
                _ = block.append(.{ .push_param = ref });
            }

            const @"fn" = self.emitExpression(loc, n.@"fn");
            break :node block.append(.{ .call = @"fn" });
        },
        .FnDecl => |_| node: {
            const ref = self.emit_fn(node);
            break :node block.append(.{ .fn_ref = ref });
        },
        else => @panic(@tagName(node.*)),
    };
}

fn getIdent(_: *Self, node: AST.NodeRef) ?[]const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => null,
    };
}

fn emitStmt(
    self: *Self,
    loc: Location,
    node: AST.NodeRef,
) void {
    const block = self.get(loc);

    switch (node.*) {
        .ConstDecl => |n| {
            const ident = self.getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emitExpression(loc, n.value);

            _ = block.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        .VarDecl => |n| {
            const ident = self.getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emitExpression(loc, n.value);

            _ = block.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        .Assignment => |n| {
            const ident = self.getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emitExpression(loc, n.value);

            _ = block.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        else => {},
    }
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                for (lst.items) |n| {
                    self.emitStmt(self.global_block, n);
                }
            },
            else => unreachable,
        }
    }
}
