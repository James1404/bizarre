const std = @import("std");
const Allocator = std.mem.Allocator;

const UIR = @import("uir.zig");
const AST = @import("ast.zig");
const Scopes = @import("scopes.zig");

allocator: Allocator,
code: UIR,
scopes: Scopes,
ast: AST,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    return Self{
        .allocator = allocator,
        .code = .make(allocator),
        .scopes = .make(allocator),
        .ast = ast,
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
    self.scopes.deinit();
}

fn emit_fn(self: *Self, node: AST.NodeRef) UIR.FnIndex {
    const @"fn": UIR.Fn = switch (node.*) {
        .FnDecl => |decl| node: {
            const ret = self.emit_as_block(decl.ret);
            const params = lst: {
                var list: std.ArrayList(UIR.Fn.Param) = .init(self.allocator);
                for (decl.params.ParamaterList.items) |param| {
                    list.append(.{
                        .ty = self.emit_as_block(param.Paramater.type),
                        .name = getIdent(param.Paramater.ident) orelse unreachable,
                    }) catch unreachable;
                }

                break :lst list;
            };
            const body = self.emit_as_block(decl.block);

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

fn getIdent(node: AST.NodeRef) ?[]const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => null,
    };
}

fn append(self: *Self, instruction: UIR.Inst) UIR.Ref {
    const idx = self.code.instructions.items.len;
    self.code.instructions.append(instruction) catch unreachable;
    return @enumFromInt(idx);
}

fn emit_as_block(self: *Self, node: AST.NodeRef) UIR.Index {
    const start = self.code.len();

    _ = self.emit(node);

    return self.append(.{ .block = UIR.BasicBlock{
        .len = self.code.len() - start,
    } }).toIndex();
}

fn emit(
    self: *Self,
    node: AST.NodeRef,
) UIR.Ref {
    return switch (node.*) {
        .Unary => |n| node: {
            const value = self.emit(n.node);
            break :node switch (n.Op.ty) {
                .Minus => self.append(.{ .negate = .{
                    .value = value,
                } }),
                else => unreachable,
            };
        },
        .Binary => |n| node: {
            const lhs = self.emit(n.lhs);
            const rhs = self.emit(n.rhs);

            break :node switch (n.Op.ty) {
                .Plus => self.append(.{ .add = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Minus => self.append(.{ .sub = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Asterix => self.append(.{ .mul = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Slash => self.append(.{ .div = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                .Less => self.append(.{ .cmp_lt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .LessEq => self.append(.{ .cmp_le = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .Greater => self.append(.{ .cmp_gt = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),
                .GreaterEq => self.append(.{ .cmp_ge = .{
                    .lhs = lhs,
                    .rhs = rhs,
                } }),

                else => unreachable,
            };
        },
        .String => |v| self.append(.{ .string = v }),
        .Int => |v| self.append(.{ .int = v }),
        .Float => |v| self.append(.{ .float = v }),
        .Bool => |v| self.append(.{ .bool = v }),
        .Ident => |v| node: {
            break :node self.append(.{ .load = .{
                .identifier = v.text,
            } });
        },
        .FnCall => |n| node: {
            for (n.args.items) |arg| {
                const ref = self.emit(arg);
                _ = self.append(.{ .push = ref });
            }

            const @"fn" = self.emit(n.@"fn");
            break :node self.append(.{ .call = @"fn" });
        },
        .FnDecl => |_| node: {
            const ref = self.emit_fn(node);
            break :node self.append(.{ .fn_ref = ref });
        },
        .Struct => |n| node: {
            const struct_index = self.append(.{ .@"struct" = .{
                .fields = .init(self.allocator),
                .decls = .init(self.allocator),
            } });
            const @"struct" = &self.code.getInstruction(
                struct_index.toIndex(),
            ).@"struct";

            for (n.fields.items) |field| switch (field.*) {
                .Field => |f| {
                    const ident = getIdent(f.ident) orelse unreachable;
                    const ty = self.emit_as_block(f.type orelse unreachable);

                    @"struct".fields.put(ident, ty) catch unreachable;
                },
                .ConstDecl => |c| {
                    const ident = getIdent(c.ident) orelse unreachable;
                    if (c.type) |ty| _ = self.emit_as_block(ty);

                    const value = self.emit_as_block(c.value);

                    @"struct".decls.put(ident, value) catch unreachable;
                },
                else => |f| @panic(@tagName(f)),
            };

            break :node struct_index;
        },
        .Comptime => self.append(.todo),

        .ConstDecl => |n| node: {
            const ident = getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emit(n.value);

            break :node self.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        .VarDecl => |n| node: {
            const ident = getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emit(n.value);

            break :node self.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },
        .Assignment => |n| node: {
            const ident = getIdent(n.ident) orelse "Unknown identifier";
            const value = self.emit(n.value);

            break :node self.append(.{ .set = .{
                .identifier = ident,
                .value = value,
            } });
        },

        .Scope => self.append(.todo),
        .Interface => self.append(.todo),
        else => @panic(@tagName(node.*)),
    };
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                const global_index = self.append(.{ .namespace = .{
                    .decls = .init(self.allocator),
                } });
                const global = &self.code.getInstruction(
                    global_index.toIndex(),
                ).namespace;

                for (lst.items) |item| _ = switch (item.*) {
                    .Comptime => self.append(.todo),
                    .VarDecl => |n| {
                        const ident = getIdent(n.ident) orelse unreachable;
                        if (n.type) |ty| _ = self.emit_as_block(ty);

                        const value = self.emit_as_block(n.value);

                        global.decls.put(ident, value) catch unreachable;
                    },
                    .ConstDecl => |n| {
                        const ident = getIdent(n.ident) orelse unreachable;
                        if (n.type) |ty| _ = self.emit_as_block(ty);

                        const value = self.emit_as_block(n.value);

                        global.decls.put(ident, value) catch unreachable;
                    },
                    else => |f| @panic(@tagName(f)),
                };
            },
            else => unreachable,
        }
    }
}

//pub fn print_index(self: *Self, writer: anytype) !void {}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    for (self.code.functions.items, 0..) |@"fn", fn_index| {
        try writer.print("fn {d} {{\n", .{fn_index});
        try writer.print("\treturn = {d}\n", .{@"fn".return_type});

        try writer.print("\tparams(", .{});
        for (@"fn".parameters.items) |param| {
            try writer.print("{s}: {d}", .{ param.name, param.ty });
        }
        try writer.print(")\n", .{});
        try writer.print("\tbody = {d}\n", .{@"fn".body});

        try writer.print("}}\n", .{});
    }

    for (self.code.instructions.items, 0..) |inst, index| {
        const instStr = std.fmt.allocPrint(
            self.allocator,
            "{s}",
            .{inst},
        ) catch |err| {
            @panic(@errorName(err));
        };
        defer self.allocator.free(instStr);
        try writer.print("%{d} = {s};\n", .{ index, instStr });
    }
}

pub fn print(self: *Self) void {
    const out = std.debug.print;

    out("{s}\n", .{self.*});
}
