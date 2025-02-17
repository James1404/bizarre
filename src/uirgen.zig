const std = @import("std");
const Allocator = std.mem.Allocator;

const UIR = @import("uir.zig");
const AST = @import("ast.zig");

allocator: Allocator,
code: UIR,
ast: AST,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    return Self{
        .allocator = allocator,
        .code = .make(allocator),
        .ast = ast,
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
}

fn emit_fn(self: *Self, node: AST.NodeRef) UIR.Fn.Index {
    const @"fn": UIR.Fn = switch (node.*) {
        .FnDecl => |decl| node: {
            const ret = self.emit_as_chunk(decl.ret);
            const params = lst: {
                var list: std.ArrayList(UIR.Fn.Param) = .init(self.allocator);
                for (decl.params.ParamaterList.items) |param| {
                    list.append(.{
                        .ty = self.emit_as_chunk(param.Paramater.type),
                        .name = getIdent(param.Paramater.ident),
                    }) catch unreachable;
                }

                break :lst list;
            };
            const body = self.emit_as_chunk(decl.block);

            break :node UIR.Fn{
                .parameters = params,
                .return_type = ret,
                .body = body,
            };
        },
        else => unreachable,
    };

    const idx = self.code.functions.items.len;
    self.code.functions.append(@"fn") catch |err| {
        @panic(@errorName(err));
    };
    return @enumFromInt(idx);
}

fn getIdent(node: AST.NodeRef) []const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => unreachable,
    };
}

fn emit_as_chunk(self: *Self, node: AST.NodeRef) UIR.Chunk.Index {
    const index = self.code.append(.{
        .instructions = .init(self.allocator),
    });

    _ = self.emitExpression(index, node);

    return index;
}

fn emitExpression(
    self: *Self,
    loc: UIR.Chunk.Index,
    node: AST.NodeRef,
) UIR.Ref {
    const chunk = self.code.get(loc);

    return switch (node.*) {
        .Unary => |n| node: {
            const value = self.emitExpression(loc, n.node);
            break :node switch (n.Op.ty) {
                .Minus => chunk.append(.{ .negate = .{
                    .value = value,
                } }),
                else => unreachable,
            };
        },
        .Binary => |n| node: {
            const lhs = self.emitExpression(loc, n.lhs);
            const rhs = self.emitExpression(loc, n.rhs);

            break :node switch (n.Op.ty) {
                .Plus => chunk.append(.{ .add = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Minus => chunk.append(.{ .sub = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Asterix => chunk.append(.{ .mul = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Slash => chunk.append(.{ .div = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                .Less => chunk.append(.{ .cmp_lt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .LessEq => chunk.append(.{ .cmp_le = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Greater => chunk.append(.{ .cmp_gt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .GreaterEq => chunk.append(.{ .cmp_ge = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                else => unreachable,
            };
        },
        .String => |v| chunk.append(.{ .string = v }),
        .Int => |v| chunk.append(.{ .int = v }),
        .Float => |v| chunk.append(.{ .float = v }),
        .Bool => |v| chunk.append(.{ .bool = v }),
        .Ident => |v| node: {
            break :node chunk.append(.{ .load = .{
                .identifier = v.text,
            } });
        },
        .FnCall => |n| node: {
            var args: std.ArrayList(UIR.Ref) = .init(self.allocator);
            for (n.args.items) |arg| {
                const ref = self.emitExpression(loc, arg);
                args.append(ref) catch unreachable;
            }

            const @"fn" = self.emitExpression(loc, n.@"fn");
            break :node chunk.append(.{ .call = .{
                .@"fn" = @"fn",
                .args = args,
            } });
        },
        .FnDecl => |_| node: {
            const ref = self.emit_fn(node);
            break :node chunk.append(.{ .fn_ref = ref });
        },
        .Struct => |n| node: {
            const struct_index = chunk.append(.{ .@"struct" = .{
                .fields = .init(self.allocator),
                .decls = .init(self.allocator),
            } });
            const @"struct" = &chunk.get(struct_index).@"struct";

            for (n.fields.items) |field| switch (field.*) {
                .Field => |f| {
                    const ident = getIdent(f.ident);
                    const ty = self.emit_as_chunk(f.type orelse unreachable);

                    @"struct".fields.put(ident, ty) catch unreachable;
                },
                .ConstDecl => |c| {
                    const ident = getIdent(c.ident);
                    if (c.type) |ty| _ = self.emit_as_chunk(ty);

                    const value = self.emit_as_chunk(c.value);

                    @"struct".decls.put(ident, value) catch unreachable;
                },
                else => |f| @panic(@tagName(f)),
            };

            break :node struct_index;
        },
        .Comptime => chunk.append(.todo),
        .Scope => |lst| node: {
            const scope_chunk = self.code.append(.{
                .instructions = .init(self.allocator),
            });

            for (lst.items) |n| {
                _ = self.emitStmt(scope_chunk, n);
            }

            break :node chunk.append(.{ .scope = scope_chunk });
        },
        .If => |n| node: {
            const cond = self.emitExpression(loc, n.cond);

            const @"if" = chunk.append(.{ .@"if" = .{
                .cond = cond,
                .true_len = 0,
                .false_len = 0,
            } });
            const if_inst = &chunk.get(@"if").@"if";

            if_inst.true_len = len: {
                const start = chunk.instructions.items.len;
                _ = self.emitExpression(loc, n.true);
                break :len chunk.instructions.items.len - start;
            };

            if_inst.false_len = len: {
                const start = chunk.instructions.items.len;
                _ = self.emitExpression(loc, n.false);
                break :len chunk.instructions.items.len - start;
            };

            break :node @"if";
        },
        else => @panic(@tagName(node.*)),
    };
}

fn emitStmt(
    self: *Self,
    loc: UIR.Chunk.Index,
    node: AST.NodeRef,
) void {
    const chunk = self.code.get(loc);

    switch (node.*) {
        .ConstDecl => |n| {
            const ident = getIdent(n.ident);
            const value = self.emitExpression(loc, n.value);

            _ = chunk.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        .VarDecl => |n| {
            const ident = getIdent(n.ident);
            const value = self.emitExpression(loc, n.value);

            _ = chunk.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        .Assignment => |n| {
            const ident = getIdent(n.ident);
            const value = self.emitExpression(loc, n.value);

            _ = chunk.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        .Return => |n| {
            const value = self.emitExpression(loc, n);
            _ = chunk.append(.{ .@"return" = .{ .value = value } });
        },
        .ImplicitReturn => |n| {
            const value = self.emitExpression(loc, n);
            _ = chunk.append(.{ .@"return" = .{ .value = value } });
        },
        else => @panic(@tagName(node.*)),
    }
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                const global_index = self.code.append(.{
                    .instructions = .init(self.allocator),
                });
                const global_chunk = self.code.get(global_index);

                const global_ref = global_chunk.append(.{ .namespace = .{
                    .decls = .init(self.allocator),
                } });

                const global = &global_chunk.get(global_ref).namespace;

                for (lst.items) |item| _ = switch (item.*) {
                    .Comptime => global_chunk.append(.todo),
                    .VarDecl => |n| {
                        const ident = getIdent(n.ident);
                        if (n.type) |ty| _ = self.emit_as_chunk(ty);

                        const value = self.emit_as_chunk(n.value);
                        global.decls.put(ident, value) catch unreachable;
                    },
                    .ConstDecl => |n| {
                        const ident = getIdent(n.ident);
                        if (n.type) |ty| _ = self.emit_as_chunk(ty);

                        const value = self.emit_as_chunk(n.value);
                        global.decls.put(ident, value) catch unreachable;
                    },
                    else => |f| @panic(@tagName(f)),
                };
            },
            else => unreachable,
        }
    }
}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    for (self.code.functions.items, 0..) |@"fn", fn_index| {
        try writer.print("fn {d} {{\n", .{fn_index});
        try writer.print("\treturn = {s}\n", .{@"fn".return_type});

        try writer.print("\tparams(", .{});
        for (@"fn".parameters.items) |param| {
            try writer.print("{s}: {s}", .{ param.name, param.ty });
        }
        try writer.print(")\n", .{});
        try writer.print("\tbody = {s}\n", .{@"fn".body});

        try writer.print("}}\n", .{});
    }

    for (self.code.chunks.items, 0..) |chunk, index| {
        try writer.print("chunk {d} {{\n", .{index});
        for (chunk.instructions.items, 0..) |inst, ref| {
            try writer.print("\t%{d} = {s};\n", .{ ref, inst });
        }
        try writer.print("}}\n", .{});
    }
}

pub fn print(self: *Self) void {
    const out = std.debug.print;

    out("{s}\n", .{self.*});
}
