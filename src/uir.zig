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

    todo,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .add => |v| try writer.print("{d} + {d}", .{ v.lhs, v.rhs }),
            .sub => |v| try writer.print("{d} - {d}", .{ v.lhs, v.rhs }),
            .mul => |v| try writer.print("{d} * {d}", .{ v.lhs, v.rhs }),
            .div => |v| try writer.print("{d} / {d}", .{ v.lhs, v.rhs }),

            .cmp_lt => |v| try writer.print("{d} < {d}", .{ v.lhs, v.rhs }),
            .cmp_gt => |v| try writer.print("{d} > {d}", .{ v.lhs, v.rhs }),
            .cmp_le => |v| try writer.print("{d} <= {d}", .{ v.lhs, v.rhs }),
            .cmp_ge => |v| try writer.print("{d} >= {d}", .{ v.lhs, v.rhs }),
            .cmp_eq => |v| try writer.print("{d} == {d}", .{ v.lhs, v.rhs }),
            .cmp_ne => |v| try writer.print("{d} != {d}", .{ v.lhs, v.rhs }),

            .negate => |v| try writer.print("-{d}", .{v.value}),

            .cast => |v| try writer.print("cast({d}, {d})", .{ v.value, v.type }),

            .assign => |v| try writer.print("assign({d})", .{v.value}),

            .load => |v| try writer.print("load({s})", .{v.identifier}),
            .set => |v| try writer.print("set({s}, {d})", .{ v.identifier, v.value }),

            .int => |v| try writer.print("int({s})", .{v}),
            .float => |v| try writer.print("float({s})", .{v}),
            .string => |v| try writer.print("string(\"{s}\")", .{v}),
            .bool => |v| try writer.print("bool({s})", .{if (v) "true" else "false"}),
            .fn_ref => |v| try writer.print("{s}", .{v}),

            .push_param => |v| try writer.print("push_param({s})", .{v}),
            .pop_param => |v| try writer.print("pop_param({s})", .{v.addr}),

            .call => |v| try writer.print("call({s})", .{v}),

            .todo => try writer.print("todo", .{}),
        }
    }
};

pub const FnIndex = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("fn({d})", .{@intFromEnum(self)});
    }
};
pub const Fn = struct {
    const Param = struct {
        ty: Ref,
        name: []const u8,
    };

    params: std.ArrayList(Param),
    ret: Ref,
    start: Location,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("fn({d})", .{@intFromEnum(self)});
    }
};

pub const Ref = enum(usize) {
    _,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("%{d}", .{@intFromEnum(self)});
    }
};

pub const Terminator = union(enum) {
    @"if": struct { cond: Ref, true: Location, false: Location },
    @"switch": struct {
        value: Ref,
        patterns: std.ArrayList(Ref),
    },
    goto: Location,
    @"return",

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .@"if" => |n| try writer.print("if({d}) {d} else {d}", .{
                n.cond,
                n.true,
                n.false,
            }),
            .@"switch" => |n| try writer.print("switch({d}) {{ {s} }}", .{
                n.value,
                n.patterns.items,
            }),

            .goto => |n| try writer.print("goto({d})", .{n}),
            .@"return" => try writer.print("return", .{}),
        }
    }
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
        .FnDecl => |decl| node: {
            const block = self.new_block();

            switch (decl.block.*) {
                .Scope => |lst| {
                    for (lst.items) |n| {
                        self.emitStmt(block, n);
                    }
                },
                else => unreachable,
            }

            break :node Fn{
                .params = .init(self.allocator),
                .ret = @enumFromInt(0),
                .start = block,
            };
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
        .Struct => block.append(.todo),
        .Comptime => block.append(.todo),
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

pub fn print(self: *Self) void {
    const out = std.debug.print;

    for (self.functions.items, 0..) |@"fn", idx| {
        out("fn {d} {{\n", .{idx});

        out("\tparams = {{\n", .{});
        for (@"fn".params.items) |param| {
            out("\t\t{s}: {d}\n", .{ param.name, param.ty });
        }
        out("\t}}\n", .{});

        out("\tret = {d}\n", .{@"fn".ret});
        out("\tblock = {d}\n", .{@"fn".start});
        out("}}\n", .{});
    }

    for (self.blocks.items, 0..) |block, idx| {
        out("block {d} {{\n", .{idx});

        for (block.instructions.items, 0..) |inst, ref| {
            const instStr = std.fmt.allocPrint(
                self.allocator,
                "{s}",
                .{inst},
            ) catch |err| {
                @panic(@errorName(err));
            };
            defer self.allocator.free(instStr);
            out("\t%{d} = {s};\n", .{ ref, instStr });
        }

        if (block.terminator) |terminator| {
            out("\t{s};\n", .{terminator});
        }

        out("}}\n", .{});
    }
}
