const std = @import("std");
const Allocator = std.mem.Allocator;

const UIR = @import("uir.zig");
const AST = @import("ast.zig");

allocator: Allocator,
ast: AST,

code: UIR,
scopes: Scopes,
register_count: u32 = 0,

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

pub fn register(self: *Self, ident: []const u8) UIR.Variable {
    const idx = self.code.register_variable(.{ .name = ident });
    self.scopes.add(ident, idx);
    return idx;
}

pub fn get_variable(self: *Self, ident: []const u8) UIR.Variable {
    if (self.scopes.get(ident)) |local| {
        return local;
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

fn emit_as_constant(self: *Self, value: UIR.Constants.Value) UIR.Ref {
    const reg = self.temp();
    const index = self.code.constants.append(value);

    _ = self.code.append(.{ .load_constant = .{
        .dest = reg,
        .index = index,
    } });

    return reg;
}

fn infer(self: *Self, ref: UIR.Ref) UIR.Ref {
    const reg = self.temp();
    _ = self.code.append(.{ .typeof = .{ .dest = reg, .value = ref } });
    return reg;
}

fn temp(self: *Self) UIR.Ref {
    const idx = self.register_count;
    self.register_count += 1;
    return @enumFromInt(idx);
}

fn emit_as_block(self: *Self, node: AST.NodeRef) UIR.Ref {
    const reg = self.temp();

    const inst = self.code.append(.{ .block = .{ .dest = reg, .len = 0 } });

    const start = self.code.len();

    _ = self.emit_expr(node);

    self.code.get(inst).comptime_block.len = self.code.len() - start;

    return reg;
}

fn emit_as_comptime_block(self: *Self, node: AST.NodeRef) UIR.Ref {
    const reg = self.temp();

    const inst = self.code.append(.{ .comptime_block = .{ .dest = reg, .len = 0 } });

    const start = self.code.len();

    _ = self.emit_expr(node);

    self.code.get(inst).comptime_block.len = self.code.len() - start;

    return reg;
}

fn emit_statement(self: *Self, node: AST.NodeRef) void {
    switch (node.*) {
        .ConstDecl => |n| {
            const ident = self.register(getIdent(n.ident));
            const value = self.emit_expr(n.value);
            const @"type" = if (n.type) |ty| self.emit_expr(ty) else self.infer(value);

            _ = self.code.append(.{ .create_const = .{ .variable = ident, .ty = @"type" } });
            _ = self.code.append(.{ .store = .{
                .variable = ident,
                .value = value,
            } });
        },
        .VarDecl => |n| {
            const ident = self.register(getIdent(n.ident));
            const value = self.emit_expr(n.value);
            const @"type" = if (n.type) |ty| self.emit_expr(ty) else self.infer(value);

            _ = self.code.append(.{ .create_var = .{ .variable = ident, .ty = @"type" } });
            _ = self.code.append(.{ .store = .{
                .variable = ident,
                .value = value,
            } });
        },
        .Assignment => |n| {
            const ident = self.get_variable(getIdent(n.ident));
            const value = self.emit_expr(n.value);

            _ = self.code.append(.{ .store = .{
                .variable = ident,
                .value = value,
            } });
        },

        .Return => |n| {
            _ = self.code.append(.{ .return_fn = .{
                .value = self.emit_expr(n),
            } });
        },
        .ImplicitReturn => |n| {
            _ = self.code.append(.{ .return_implicit = .{
                .value = self.emit_expr(n),
            } });
        },

        .If => |n| {
            const cond = self.emit_expr(n.cond);

            const inst = self.code.append(.{ .if_stmt = .{
                .cond = cond,
                .true_len = 0,
                .false_len = 0,
            } });

            {
                const start = self.code.len();
                self.scopes.down();

                for (n.true.Scope.items) |item| {
                    self.emit_statement(item);
                }

                self.scopes.up();
                self.code.get(inst).if_expr.true_len = self.code.len() - start;
            }

            {
                const start = self.code.len();
                self.scopes.down();

                for (n.false.Scope.items) |item| {
                    self.emit_statement(item);
                }

                self.scopes.up();
                self.code.get(inst).if_expr.false_len = self.code.len() - start;
            }
        },

        else => _ = self.emit_expr(node),
    }
}

fn emit_expr(
    self: *Self,
    node: AST.NodeRef,
) UIR.Ref {
    return switch (node.*) {
        .Unary => |n| reg: {
            const value = self.emit_expr(n.node);
            const reg = self.temp();
            _ = switch (n.Op.ty) {
                .Minus => self.code.append(.{ .negative = .{ .dest = reg, .value = value } }),
                else => unreachable,
            };
            break :reg reg;
        },
        .Binary => |n| reg: {
            const lhs = self.emit_expr(n.lhs);
            const rhs = self.emit_expr(n.rhs);
            const reg = self.temp();

            _ = switch (n.Op.ty) {
                .Plus => self.code.append(.{ .add = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Minus => self.code.append(.{ .sub = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Asterix => self.code.append(.{ .mul = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Slash => self.code.append(.{ .div = .{ .dest = reg, .l = lhs, .r = rhs } }),

                .Less => self.code.append(.{ .cmp_lt = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .LessEq => self.code.append(.{ .cmp_le = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .Greater => self.code.append(.{ .cmp_gt = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .GreaterEq => self.code.append(.{ .cmp_ge = .{ .dest = reg, .l = lhs, .r = rhs } }),

                .EqualEqual => self.code.append(.{ .cmp_eq = .{ .dest = reg, .l = lhs, .r = rhs } }),
                .NotEqual => self.code.append(.{ .cmp_ne = .{ .dest = reg, .l = lhs, .r = rhs } }),

                else => unreachable,
            };
            break :reg reg;
        },
        .String => |v| self.emit_as_constant(.{ .string = v }),
        .Int => |v| self.emit_as_constant(.{ .int = v }),
        .Float => |v| self.emit_as_constant(.{ .float = v }),
        .Bool => |v| self.emit_as_constant(.{ .bool = v }),
        .Ident => |v| reg: {
            const reg = self.temp();

            if (UIR.Builtin.fromString(v.text)) |builtin| {
                _ = self.code.append(.{ .load_builtin = .{
                    .dest = reg,
                    .builtin = builtin,
                } });
            } else {
                const index = self.get_variable(v.text);
                _ = self.code.append(.{ .load = .{ .dest = reg, .variable = index } });
            }

            break :reg reg;
        },
        .FnCall => |n| reg: {
            const @"fn" = self.emit_expr(n.@"fn");

            var args: std.ArrayList(UIR.Ref) = .init(self.allocator);

            for (n.args.items) |arg| {
                args.append(self.emit_expr(arg)) catch unreachable;
            }

            const reg = self.temp();
            _ = self.code.append(.{ .call = .{
                .dest = reg,
                .@"fn" = @"fn",
                .args = args,
            } });

            break :reg reg;
        },
        .FnDecl => |n| reg: {
            self.scopes.down();

            const reg = self.temp();
            const inst = self.code.append(.{ .fn_decl = .{
                .dest = reg,
                .argc = n.params.ParamaterList.items.len,
                .len = 0,
            } });

            const start = self.code.len();

            for (n.params.ParamaterList.items, 0..) |param, idx| {
                const ident = getIdent(param.Paramater.ident);
                const param_idx = self.register(ident);
                const param_ty = self.emit_expr(param.Paramater.type);

                const arg_reg = self.temp();
                _ = self.code.append(.{ .load_arg = .{
                    .dest = arg_reg,
                    .arg = idx,
                } });

                _ = self.code.append(.{ .create_const = .{
                    .variable = param_idx,
                    .ty = param_ty,
                } });
                _ = self.code.append(.{ .store = .{
                    .variable = param_idx,
                    .value = arg_reg,
                } });
            }

            const return_type = self.emit_expr(n.ret);
            _ = self.code.append(.{ .set_return_type = .{ .ty = return_type } });

            switch (n.block.*) {
                .Scope => |lst| {
                    for (lst.items) |item| {
                        self.emit_statement(item);
                    }
                },
                else => unreachable,
            }

            self.code.get(inst).fn_decl.len = self.code.len() - start;

            self.scopes.up();

            break :reg reg;
        },
        .Struct => |n| reg: {
            const reg = self.temp();
            const inst = self.code.append(.{ .struct_decl = .{ .dest = reg, .len = 0 } });

            const start = self.code.len();

            for (n.fields.items) |field| _ = switch (field.*) {
                .Field => |f| {
                    const ident = self.register(getIdent(f.ident));
                    const ty = self.emit_expr(f.type orelse unreachable);

                    _ = self.code.append(.{ .create_field = .{ .ident = ident, .ty = ty } });
                },
                .ConstDecl => self.emit_statement(field),
                else => |f| @panic(@tagName(f)),
            };

            self.code.get(inst).struct_decl.len = self.code.len() - start;

            break :reg reg;
        },
        .Comptime => |n| self.emit_as_comptime_block(n),

        .Scope => |lst| reg: {
            const reg = self.temp();
            const inst = self.code.append(.{ .block = .{
                .dest = reg,
                .len = 0,
            } });

            const start = self.code.len();

            self.scopes.down();

            for (lst.items) |n| _ = self.emit_statement(n);

            self.scopes.up();

            self.code.get(inst).block.len = self.code.len() - start;

            break :reg reg;
        },

        .If => |n| reg: {
            const cond = self.emit_expr(n.cond);

            const reg = self.temp();
            const inst = self.code.append(.{ .if_expr = .{
                .dest = reg,
                .cond = cond,
                .true_len = 0,
                .false_len = 0,
            } });

            {
                const start = self.code.len();
                self.scopes.down();

                for (n.true.Scope.items) |item| {
                    self.emit_statement(item);
                }

                self.scopes.up();
                self.code.get(inst).if_expr.true_len = self.code.len() - start;
            }

            {
                const start = self.code.len();
                self.scopes.down();

                for (n.false.Scope.items) |item| {
                    self.emit_statement(item);
                }

                self.scopes.up();
                self.code.get(inst).if_expr.false_len = self.code.len() - start;
            }

            break :reg reg;
        },

        else => @panic(@tagName(node.*)),
    };
}

pub fn run(self: *Self) void {
    if (self.ast.root) |root| {
        switch (root.*) {
            .TopLevelScope => |lst| {
                const reg = self.temp();

                const inst = self.code.append(.{ .namespace_decl = .{ .dest = reg, .len = 0 } });

                const start = self.code.len();

                for (lst.items) |n| self.emit_statement(n);

                self.code.get(inst).namespace_decl.len = self.code.len() - start;

                self.code.registers_count = self.register_count;
            },
            else => unreachable,
        }
    }
}

pub fn print(
    self: *Self,
) !void {
    const writer = std.io.getStdOut().writer();

    try writer.print("len: {d}\n", .{self.code.len()});
    try self.code.print(writer);
}

pub const Scope = struct {
    parent: ?Index = null,
    depth: usize,

    children: std.ArrayList(Index),
    symbols: std.StringHashMap(usize),

    pub const Index = enum(usize) { _ };

    pub fn make(allocator: Allocator, depth: usize) Scope {
        return Scope{
            .parent = null,
            .depth = depth,
            .children = .init(allocator),
            .symbols = .init(allocator),
        };
    }

    pub fn makeWithParent(allocator: Allocator, depth: usize, parent: Index) Scope {
        return Scope{
            .parent = parent,
            .depth = depth,
            .children = .init(allocator),
            .symbols = .init(allocator),
        };
    }

    pub fn deinit(this: *Scope) void {
        this.parent = null;
        this.children.deinit();
        this.symbols.deinit();
    }
};

pub const Scopes = struct {
    stack: std.ArrayList(Local),
    depth: usize = 0,

    pub const Local = struct {
        name: []const u8,
        mapping: UIR.Variable,
        depth: usize,
    };

    pub fn make(allocator: Allocator) Scopes {
        return Scopes{
            .stack = .init(allocator),
        };
    }

    pub fn deinit(scopes: *Scopes) void {
        scopes.stack.deinit();
    }

    pub fn get(scopes: *Scopes, name: []const u8) ?UIR.Variable {
        const len = scopes.stack.items.len;
        if (len == 0) return null;

        var i = len - 1;

        while (i >= 0) : (i -= 1) {
            const local = scopes.stack.items[i];

            if (std.mem.eql(u8, name, local.name)) {
                return local.mapping;
            }
        }

        return null;
    }

    pub fn add(scopes: *Scopes, name: []const u8, mapping: UIR.Variable) void {
        scopes.stack.append(.{
            .name = name,
            .mapping = mapping,
            .depth = scopes.depth,
        }) catch unreachable;
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
