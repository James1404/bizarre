const std = @import("std");
const Allocator = std.mem.Allocator;

const UIR = @import("uir.zig");
const AST = @import("ast.zig");

allocator: Allocator,
ast: AST,

code: UIR,
scopes: Scopes,

const Self = @This();

pub fn make(allocator: Allocator, ast: AST) Self {
    return Self{
        .allocator = allocator,
        .ast = ast,
        .code = .make(allocator),
        .scopes = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
    self.scopes.deinit();
}

fn register_local(
    self: *Self,
    chunk: *UIR.Chunk,
    ident: []const u8,
) UIR.Ref {
    const reg = chunk.reg();
    _ = self.scopes.add(ident, .{ .ref = reg });
    return reg;
}

fn register_decl(
    self: *Self,
    ident: []const u8,
    index: UIR.Decl.Index,
) void {
    _ = self.scopes.add(ident, .{ .decl = index });
}

fn get_local(self: *Self, ident: []const u8) UIR.Ref {
    if (self.scopes.get(ident)) |local| {
        switch (local.value) {
            .ref => |ref| return ref,
            else => {},
        }
    }

    std.log.err("Variable \"{s}\" is not defined", .{ident});
    unreachable;
}

fn load_variable(
    self: *Self,
    chunk: *UIR.Chunk,
    ident: []const u8,
    out: UIR.Ref,
) void {
    if (self.scopes.get(ident)) |variable| {
        switch (variable.value) {
            .ref => |ref| _ = chunk.append(.{ .load = .{
                .dest = out,
                .ref = ref,
            } }),
            .decl => |decl| _ = chunk.append(.{ .load_decl = .{
                .dest = out,
                .index = decl,
            } }),
        }
        return;
    }

    std.log.err("Variable \"{s}\" is not defined", .{ident});
    unreachable;
}

fn set_variable(
    self: *Self,
    chunk: *UIR.Chunk,
    ident: []const u8,
    value: UIR.Ref,
) void {
    if (self.scopes.get(ident)) |variable| {
        switch (variable.value) {
            .ref => |ref| _ = chunk.append(.{ .store = .{
                .ref = ref,
                .value = value,
            } }),
            .decl => |decl| _ = chunk.append(.{ .store_decl = .{
                .index = decl,
                .value = value,
            } }),
        }

        return;
    }

    std.log.err("Variable \"{s}\" is not defined", .{ident});
    unreachable;
}

fn getIdent(node: AST.NodeRef) []const u8 {
    return switch (node.*) {
        .Ident => |tok| tok.text,
        else => unreachable,
    };
}

fn emit_as_constant(self: *Self, chunk: *UIR.Chunk, value: UIR.Constants.Value) UIR.Ref {
    const reg = chunk.reg();
    const index = self.code.constants.append(value);

    _ = chunk.append(.{ .load_constant = .{
        .dest = reg,
        .index = index,
    } });

    return reg;
}

fn infer(_: *Self, chunk: *UIR.Chunk, ref: UIR.Ref) UIR.Ref {
    const reg = chunk.reg();
    _ = chunk.append(.{ .typeof = .{ .dest = reg, .value = ref } });
    return reg;
}

fn emit_as_block(self: *Self, chunk: *UIR.Chunk, node: AST.NodeRef) UIR.Ref {
    const reg = chunk.reg();

    const inst = chunk.append(.{ .block = .{ .dest = reg, .len = 0 } });

    const start = chunk.len();

    _ = self.emit_expr(chunk, node);

    chunk.get(inst).comptime_block.len = chunk.len() - start;

    return reg;
}

fn emit_as_comptime_block(self: *Self, chunk: *UIR.Chunk, node: AST.NodeRef) UIR.Ref {
    const reg = chunk.reg();

    const inst = chunk.append(.{ .comptime_block = .{ .dest = reg, .len = 0 } });

    const start = chunk.len();

    _ = self.emit_expr(chunk, node);

    chunk.get(inst).comptime_block.len = chunk.len() - start;

    return reg;
}

fn emit_statement(self: *Self, chunk: *UIR.Chunk, node: AST.NodeRef) void {
    switch (node.*) {
        .ConstDecl => |n| {
            const ident = self.register_local(chunk, getIdent(n.ident));

            const value = self.emit_expr(chunk, n.value);
            const @"type" = if (n.type) |ty|
                self.emit_expr(chunk, ty)
            else
                self.infer(chunk, value);

            _ = chunk.append(.{ .typed_value = .{
                .dest = ident,
                .value = value,
                .ty = @"type",
            } });
        },
        .VarDecl => |n| {
            const ident = self.register_local(chunk, getIdent(n.ident));

            const value = self.emit_expr(chunk, n.value);
            const @"type" = if (n.type) |ty|
                self.emit_expr(chunk, ty)
            else
                self.infer(chunk, value);

            _ = chunk.append(.{ .typed_value = .{
                .dest = ident,
                .value = value,
                .ty = @"type",
            } });
        },
        .Assignment => |n| {
            const value = self.emit_expr(chunk, n.value);
            self.set_variable(chunk, getIdent(n.ident), value);
        },

        .Return => |n| {
            _ = chunk.append(.{ .@"return" = .{
                .value = self.emit_expr(chunk, n),
            } });
        },
        .ImplicitReturn => |n| {
            _ = chunk.append(.{ .return_implicit = .{
                .value = self.emit_expr(chunk, n),
            } });
        },

        .If => |n| {
            const cond = self.emit_expr(chunk, n.cond);

            const inst = chunk.append(.{ .if_stmt = .{
                .cond = cond,
                .true_len = 0,
                .false_len = 0,
            } });

            {
                const start = chunk.len();
                self.scopes.down();

                for (n.true.Scope.items) |item| {
                    self.emit_statement(chunk, item);
                }

                self.scopes.up();
                chunk.get(inst).if_expr.true_len = chunk.len() - start;
            }

            {
                const start = chunk.len();
                self.scopes.down();

                for (n.false.Scope.items) |item| {
                    self.emit_statement(chunk, item);
                }

                self.scopes.up();
                chunk.get(inst).if_expr.false_len = chunk.len() - start;
            }
        },

        else => _ = self.emit_expr(chunk, node),
    }
}

fn emit_global_statement(self: *Self, node: AST.NodeRef) void {
    switch (node.*) {
        .ConstDecl => |n| {
            var chunk = UIR.Chunk.make(self.allocator);

            switch (n.value.*) {
                .FnDecl => {
                    const decl = self.code.add_decl(.{
                        .chunk = chunk,
                        .mode = .Fn,
                    });
                    self.register_decl(getIdent(n.ident), decl);
                    self.emit_fn_decl(&chunk, n.value);

                    self.code.get_decl(decl).chunk = chunk;

                    if (std.mem.eql(u8, getIdent(n.ident), "main")) {
                        self.code.entry_point = decl;
                    }
                },
                else => {
                    const value = self.emit_expr(&chunk, n.value);

                    const @"type" = if (n.type) |ty|
                        self.emit_expr(&chunk, ty)
                    else
                        self.infer(&chunk, value);

                    const ret = chunk.reg();
                    _ = chunk.append(.{ .typed_value = .{
                        .dest = ret,
                        .value = value,
                        .ty = @"type",
                    } });
                    _ = chunk.append(.{ .@"return" = .{ .value = ret } });

                    const decl = self.code.add_decl(.{ .chunk = chunk, .mode = .Const });
                    self.register_decl(getIdent(n.ident), decl);
                },
            }
        },
        .VarDecl => |n| {
            var chunk = UIR.Chunk.make(self.allocator);

            switch (n.value.*) {
                .FnDecl => {
                    const decl = self.code.add_decl(.{
                        .chunk = chunk,
                        .mode = .Fn,
                    });
                    self.register_decl(getIdent(n.ident), decl);
                    self.emit_fn_decl(&chunk, n.value);
                },
                else => {
                    const value = self.emit_expr(&chunk, n.value);

                    const @"type" = if (n.type) |ty|
                        self.emit_expr(&chunk, ty)
                    else
                        self.infer(&chunk, value);

                    const ret = chunk.reg();
                    _ = chunk.append(.{ .typed_value = .{
                        .dest = ret,
                        .value = value,
                        .ty = @"type",
                    } });
                    _ = chunk.append(.{ .@"return" = .{ .value = ret } });

                    const decl = self.code.add_decl(.{ .chunk = chunk, .mode = .Var });
                    self.register_decl(getIdent(n.ident), decl);
                },
            }
        },
        else => std.debug.panic(
            "Invalid Toplevel statement: {s}",
            .{@tagName(node.*)},
        ),
    }
}

fn emit_fn_decl(
    self: *Self,
    chunk: *UIR.Chunk,
    node: AST.NodeRef,
) void {
    const n = node.FnDecl;
    self.scopes.down();

    _ = chunk.append(.{ .set_argc = .{
        .len = n.params.ParamaterList.items.len,
    } });

    for (n.params.ParamaterList.items, 0..) |param, idx| {
        const arg_reg = chunk.reg();
        _ = chunk.append(.{ .load_arg = .{
            .dest = arg_reg,
            .arg = idx,
        } });

        const param_ty = self.emit_expr(chunk, param.Paramater.type);

        const ident = getIdent(param.Paramater.ident);
        const param_reg = self.register_local(chunk, ident);
        _ = chunk.append(.{ .typed_value = .{
            .dest = param_reg,
            .value = arg_reg,
            .ty = param_ty,
        } });
    }

    const return_type = self.emit_expr(chunk, n.ret);
    _ = chunk.append(.{ .set_return_type = .{ .ty = return_type } });

    switch (n.block.*) {
        .Scope => |lst| {
            for (lst.items) |item| {
                self.emit_statement(chunk, item);
            }
        },
        else => unreachable,
    }

    self.scopes.up();
}

fn emit_expr(
    self: *Self,
    chunk: *UIR.Chunk,
    node: AST.NodeRef,
) UIR.Ref {
    return switch (node.*) {
        .Unary => |n| reg: {
            const value = self.emit_expr(chunk, n.node);
            const reg = chunk.reg();
            _ = switch (n.Op.ty) {
                .Minus => chunk.append(.{ .negative = .{ .dest = reg, .value = value } }),
                else => unreachable,
            };
            break :reg reg;
        },
        .Binary => |n| reg: {
            const lhs = self.emit_expr(chunk, n.lhs);
            const rhs = self.emit_expr(chunk, n.rhs);
            const reg = chunk.reg();

            _ = switch (n.Op.ty) {
                .Plus => chunk.append(.{ .add = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Minus => chunk.append(.{ .sub = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Asterix => chunk.append(.{ .mul = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Slash => chunk.append(.{ .div = .{ .dest = reg, .l = lhs, .r = rhs } }),

                .Less => chunk.append(.{ .cmp_lt = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .LessEq => chunk.append(.{ .cmp_le = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Greater => chunk.append(.{ .cmp_gt = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .GreaterEq => chunk.append(.{ .cmp_ge = .{ .dest = reg, .l = lhs, .r = rhs } }),

                .EqualEqual => chunk.append(.{ .cmp_eq = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .NotEqual => chunk.append(.{ .cmp_ne = .{ .dest = reg, .l = lhs, .r = rhs } }),

                else => unreachable,
            };
            break :reg reg;
        },
        .String => |v| self.emit_as_constant(chunk, .{ .string = v }),
        .Int => |v| self.emit_as_constant(chunk, .{ .int = v }),
        .Float => |v| self.emit_as_constant(chunk, .{ .float = v }),
        .Bool => |v| self.emit_as_constant(chunk, .{ .bool = v }),
        .Ident => |v| reg: {
            const reg = chunk.reg();

            if (UIR.Builtin.fromString(v.text)) |builtin| {
                _ = chunk.append(.{ .load_builtin = .{
                    .dest = reg,
                    .builtin = builtin,
                } });
            } else {
                self.load_variable(chunk, v.text, reg);
            }

            break :reg reg;
        },
        .FnCall => |n| reg: {
            const @"fn" = self.emit_expr(chunk, n.@"fn");

            var args: std.ArrayList(UIR.Ref) = .init(self.allocator);

            for (n.args.items) |arg| {
                args.append(self.emit_expr(chunk, arg)) catch unreachable;
            }

            const reg = chunk.reg();
            _ = chunk.append(.{ .call = .{
                .dest = reg,
                .@"fn" = @"fn",
                .args = args,
            } });

            break :reg reg;
        },
        .FnDecl => |_| reg: {
            var body = UIR.Chunk.make(self.allocator);

            self.emit_fn_decl(&body, node);

            const index = self.code.add_decl(.{
                .chunk = body,
                .mode = .Fn,
            });

            const reg = chunk.reg();
            _ = chunk.append(.{ .load_decl = .{
                .dest = reg,
                .index = index,
            } });

            break :reg reg;
        },
        .Struct => |n| reg: {
            const reg = chunk.reg();
            const inst = chunk.append(.{ .struct_decl = .{ .dest = reg, .len = 0 } });

            const start = chunk.len();

            var field_len: usize = 0;
            for (n.fields.items) |field| _ = switch (field.*) {
                .Field => |f| {
                    const ty = self.emit_expr(chunk, f.type orelse unreachable);
                    _ = chunk.append(.{ .create_field = .{
                        .index = field_len,
                        .ty = ty,
                    } });
                    field_len += 1;
                },
                .ConstDecl => self.emit_statement(chunk, field),
                else => |f| @panic(@tagName(f)),
            };

            chunk.get(inst).struct_decl.len = chunk.len() - start;

            break :reg reg;
        },
        .Comptime => |n| self.emit_as_comptime_block(chunk, n),

        .Scope => |lst| reg: {
            const reg = chunk.reg();
            const inst = chunk.append(.{ .block = .{
                .dest = reg,
                .len = 0,
            } });

            const start = chunk.len();

            self.scopes.down();

            for (lst.items) |n| _ = self.emit_statement(chunk, n);

            self.scopes.up();

            chunk.get(inst).block.len = chunk.len() - start;

            break :reg reg;
        },

        .If => |n| reg: {
            const cond = self.emit_expr(chunk, n.cond);

            const reg = chunk.reg();
            const inst = chunk.append(.{ .if_expr = .{
                .dest = reg,
                .cond = cond,
                .true_len = 0,
                .false_len = 0,
            } });

            {
                const start = chunk.len();
                self.scopes.down();

                for (n.true.Scope.items) |item| {
                    self.emit_statement(chunk, item);
                }

                self.scopes.up();
                chunk.get(inst).if_expr.true_len = chunk.len() - start;
            }

            {
                const start = chunk.len();
                self.scopes.down();

                for (n.false.Scope.items) |item| {
                    self.emit_statement(chunk, item);
                }

                self.scopes.up();
                chunk.get(inst).if_expr.false_len = chunk.len() - start;
            }

            break :reg reg;
        },

        else => @panic(@tagName(node.*)),
    };
}

pub fn global_pass(_: *Self) void {}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                for (lst.items) |n| self.emit_global_statement(n);
            },
            else => unreachable,
        }
    }
}

pub fn print(
    self: *Self,
) !void {
    const writer = std.io.getStdOut().writer();
    try self.code.print(writer);
}

pub const Scopes = struct {
    stack: std.ArrayList(Binding),
    depth: usize = 0,

    pub const Binding = struct {
        name: []const u8,
        depth: usize,
        value: Value,

        pub const Value = union(enum) {
            ref: UIR.Ref,
            decl: UIR.Decl.Index,
        };

        pub const Index = enum(usize) { _ };
    };

    pub fn make(allocator: Allocator) Scopes {
        return Scopes{
            .stack = .init(allocator),
        };
    }

    pub fn deinit(scopes: *Scopes) void {
        scopes.stack.deinit();
    }

    pub fn get(scopes: *Scopes, name: []const u8) ?Binding {
        const len = scopes.stack.items.len;
        if (len == 0) return null;

        var i: i32 = @intCast(len - 1);

        while (i >= 0) : (i -= 1) {
            const local = scopes.stack.items[@intCast(i)];

            if (std.mem.eql(u8, name, local.name)) {
                return local;
            }
        }

        return null;
    }

    pub fn add(
        scopes: *Scopes,
        name: []const u8,
        value: Binding.Value,
    ) Binding.Index {
        const index = scopes.stack.items.len;
        scopes.stack.append(.{
            .name = name,
            .value = value,
            .depth = scopes.depth,
        }) catch unreachable;
        return @enumFromInt(index);
    }

    pub fn down(scopes: *Scopes) void {
        scopes.depth += 1;
    }

    pub fn up(scopes: *Scopes) void {
        scopes.depth -= 1;

        while (scopes.stack.items.len > 0 and
            scopes.stack.getLast().depth > scopes.depth)
        {
            _ = scopes.stack.pop();
        }
    }
};
