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

fn getIdent(node: AST.NodeRef) []const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => unreachable,
    };
}

fn emit_as_constant(self: *Self, value: UIR.Constants.Value) UIR.Ref {
    const index = self.code.constants.append(value);
    return self.code.append(.{ .load_constant = .{
        .constant = index,
    } });
}

fn emit_as_block(self: *Self, node: AST.NodeRef) UIR.Ref {
    const block = self.code.append(.{ .block = .{ .len = 0 } });

    const start = self.code.len();

    _ = self.emit(node);

    self.code.get(block).block.len = self.code.len() - start;

    return block;
}

fn infer(self: *Self, ref: UIR.Ref) UIR.Ref {
    return self.code.append(.{ .typeof = .{
        .value = ref,
    } });
}

fn emit_as_comptime_block(self: *Self, node: AST.NodeRef) UIR.Ref {
    const block = self.code.append(.{ .comptime_block = .{ .len = 0 } });

    const start = self.code.len();

    _ = self.emit(node);

    self.code.get(block).block.len = self.code.len() - start;

    return block;
}

fn emit(
    self: *Self,
    node: AST.NodeRef,
) UIR.Ref {
    return switch (node.*) {
        .Unary => |n| node: {
            const value = self.emit(n.node);
            break :node switch (n.Op.ty) {
                .Minus => self.code.append(.{ .negate = .{
                    .value = value,
                } }),
                else => unreachable,
            };
        },
        .Binary => |n| node: {
            const lhs = self.emit(n.lhs);
            const rhs = self.emit(n.rhs);

            break :node switch (n.Op.ty) {
                .Plus => self.code.append(.{ .add = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Minus => self.code.append(.{ .sub = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Asterix => self.code.append(.{ .mul = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Slash => self.code.append(.{ .div = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                .Less => self.code.append(.{ .cmp_lt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .LessEq => self.code.append(.{ .cmp_le = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Greater => self.code.append(.{ .cmp_gt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .GreaterEq => self.code.append(.{ .cmp_ge = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                else => unreachable,
            };
        },
        .String => |v| self.emit_as_constant(.{ .string = v }),
        .Int => |v| self.emit_as_constant(.{ .int = v }),
        .Float => |v| self.emit_as_constant(.{ .float = v }),
        .Bool => |v| self.emit_as_constant(.{ .bool = v }),
        .Ident => |v| node: {
            break :node self.code.append(.{ .load = .{
                .identifier = v.text,
            } });
        },
        .FnCall => |n| node: {
            var args: std.ArrayList(UIR.Ref) = .init(self.allocator);
            for (n.args.items) |arg| {
                args.append(self.emit(arg)) catch unreachable;
            }

            const @"fn" = self.emit(n.@"fn");
            break :node self.code.append(.{ .call = .{
                .@"fn" = @"fn",
                .args = args,
            } });
        },
        .FnDecl => |n| node: {
            const @"fn" = self.code.append(.{ .fn_decl = .{
                .return_type_len = 0,
                .body_len = 0,
            } });

            var start = self.code.len();

            for (n.params.ParamaterList.items) |param| {
                const param_ty = self.emit(param.Paramater.type);
                _ = self.code.append(.{ .define = .{
                    .identifier = getIdent(param.Paramater.ident),
                    .type = param_ty,
                    .value = null,
                    .mode = .Var,
                } });
            }

            _ = self.emit(n.ret);
            self.code.get(@"fn").fn_decl.return_type_len = self.code.len() - start;

            start = self.code.len();
            _ = self.emit(n.block);
            self.code.get(@"fn").fn_decl.body_len = self.code.len() - start;

            break :node @"fn";
        },
        .Struct => |n| node: {
            const struct_index = self.code.append(.{ .struct_decl = .{
                .fields = .init(self.allocator),
                .decls = .init(self.allocator),
            } });
            const @"struct" = &self.code.get(struct_index).struct_decl;

            for (n.fields.items) |field| switch (field.*) {
                .Field => |f| {
                    const ident = getIdent(f.ident);
                    const ty = self.emit_as_block(f.type orelse unreachable);

                    @"struct".fields.append(.{
                        .name = ident,
                        .type = ty,
                    }) catch unreachable;
                },
                .ConstDecl => |c| {
                    const ident = getIdent(c.ident);
                    if (c.type) |ty| _ = self.emit_as_block(ty);

                    const value = self.emit_as_block(c.value);

                    @"struct".decls.append(.{
                        .name = ident,
                        .value = value,
                    }) catch unreachable;
                },
                else => |f| @panic(@tagName(f)),
            };

            break :node struct_index;
        },
        .Comptime => |n| self.emit_as_comptime_block(n),

        .ConstDecl => |n| node: {
            const ident = getIdent(n.ident);
            const value = self.emit(n.value);
            const @"type" = if (n.type) |ty| self.emit(ty) else self.infer(value);

            break :node self.code.append(.{ .define = .{
                .identifier = ident,
                .type = @"type",
                .value = value,
                .mode = .Const,
            } });
        },
        .VarDecl => |n| node: {
            const ident = getIdent(n.ident);
            const value = self.emit(n.value);
            const @"type" = if (n.type) |ty| self.emit(ty) else self.infer(value);

            break :node self.code.append(.{ .define = .{
                .identifier = ident,
                .type = @"type",
                .value = value,
                .mode = .Var,
            } });
        },
        .Assignment => |n| node: {
            const ident = getIdent(n.ident);
            const value = self.emit(n.value);

            break :node self.code.append(.{ .store = .{
                .identifier = ident,
                .value = value,
            } });
        },

        .If => |n| node: {
            const cond = self.emit(n.cond);

            const @"if" = self.code.append(.{ .@"if" = .{
                .cond = cond,
                .true_len = 0,
                .false_len = 0,
            } });

            self.code.get(@"if").@"if".true_len = len: {
                const start = self.code.len();
                _ = self.emit(n.true);
                break :len self.code.len() - start;
            };

            self.code.get(@"if").@"if".false_len = len: {
                const start = self.code.len();
                _ = self.emit(n.false);
                break :len self.code.len() - start;
            };

            break :node @"if";
        },

        .Scope => |lst| node: {
            const block = self.code.append(.{ .block = .{ .len = 0 } });

            const start = self.code.len();

            for (lst.items) |n| _ = self.emit(n);

            self.code.get(block).block.len = self.code.len() - start;

            break :node block;
        },
        .Return => |n| self.code.append(.{ .return_fn = .{
            .value = self.emit(n),
        } }),
        .ImplicitReturn => |n| self.code.append(.{ .return_block = .{
            .value = self.emit(n),
        } }),

        else => @panic(@tagName(node.*)),
    };
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                const namespace = self.code.append(.{ .namespace_decl = .{
                    .body_len = 0,
                } });
                const start = self.code.len();

                for (lst.items) |n| _ = self.emit(n);

                self.code.get(namespace).namespace_decl.body_len = self.code.len() - start;
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
    try writer.print("len: {d}\n", .{self.code.instructions.items.len});
    for (self.code.instructions.items, 0..) |inst, idx| {
        const ref: UIR.Ref = @enumFromInt(idx);
        try writer.print("{s} = {s}\n", .{ ref, inst });
    }
}

pub fn print(self: *Self) void {
    const out = std.debug.print;
    out("{s}\n", .{self.*});
}
