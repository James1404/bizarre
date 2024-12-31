const std = @import("std");

const AST = @import("ast.zig");
const Token = @import("token.zig");

allocator: std.mem.Allocator,

src: Token.List,
pos: usize,
ast: AST,

const Self = @This();

pub fn make(
    allocator: std.mem.Allocator,
    src: Token.List,
) Self {
    return Self{
        .allocator = allocator,

        .src = src,
        .pos = 0,
        .ast = .make(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.ast.deinit();
}

fn alloc(self: *Self, node: AST.Node) AST.NodeRef {
    switch (node) {
        .Error => |err| {
            std.debug.panic("Error [{d}, {d}] \"{s}\": {s}", .{
                err.token.line,
                err.token.offset,
                err.token.text,
                err.msg,
            });
        },
        else => {},
    }

    return self.ast.alloc(node);
}

fn eof(self: Self) bool {
    return self.pos >= self.src.items.len;
}

fn advance(self: *Self) void {
    self.pos += 1;
}

fn getCurrent(self: *Self) Token {
    return self.src.items[self.pos];
}

fn match(self: *Self, expected: Token.Ty) bool {
    return self.getCurrent().ty == expected;
}

fn advanceIf(self: *Self, expected: Token.Ty) ?Token {
    if (self.match(expected)) {
        const tok = self.getCurrent();
        self.advance();
        return tok;
    }

    return null;
}

// fn parse_if(self: *Self) AST.NodeRef {
//     const start = self.getCurrent();

//     if (self.advanceIf(.If)) |_| {
//         const cond = self.parse_expr();

//         if (self.advanceIf(.Then)) |_| {
//             const true = self.parse_expr();

//             if (self.advanceIf(.Else)) |_| {
//                 const false = self.parse_expr();

//                 return self.alloc(.{ .If = .{
//                     .cond = cond,
//                     .true = true,
//                     .false = false,
//                 } });
//             }
//         }
//     }

//     return self.alloc(.{ .Error = .{
//         .msg = "Failed to parse if statement",
//         .token = start,
//     } });
// }

fn parse_fnDecl(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Fn)) |_| {
        if (self.advanceIf(.LParen)) |_| {
            var params = std.ArrayList(AST.NodeRef).init(self.allocator);

            while (true) {
                if (self.match(.RParen)) break;

                const ident = self.parse_ident();

                if (self.advanceIf(.Colon) == null) return self.alloc(.{ .Error = .{
                    .msg = "Argument needs a type with ':'",
                    .token = start,
                } });

                const @"type" = self.parse_expr();

                params.append(self.alloc(.{ .Paramater = .{
                    .ident = ident,
                    .type = @"type",
                } })) catch |err| {
                    @panic(@errorName(err));
                };

                if (self.advanceIf(.Colon) == null) break;
            }

            if (self.advanceIf(.RParen) == null) {
                return self.alloc(.{ .Error = .{
                    .msg = "Function paramater list requires closing brace",
                    .token = start,
                } });
            }

            const ret = self.parse_expr();

            return if (self.match(.LBrace)) node: {
                const block = self.parse_scope();

                break :node self.alloc(.{ .FnDecl = .{
                    .params = self.alloc(.{
                        .ParamaterList = params,
                    }),
                    .ret = ret,
                    .block = block,
                } });
            } else self.alloc(.{ .FnType = .{
                .params = params,
                .ret = ret,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Could'nt parse function decleration",
        .token = start,
    } });
}

fn parse_ident(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (start.ty == .Ident) {
        self.advance();

        return self.alloc(.{
            .Ident = start,
        });
    }

    return self.alloc(.{ .Error = .{
        .msg = "Unable to parse identifier",
        .token = start,
    } });
}

fn advanceAlloc(self: *Self, node: AST.Node) AST.NodeRef {
    self.advance();
    return self.alloc(node);
}

fn parse_value(self: *Self) AST.NodeRef {
    const tok = self.getCurrent();

    const value = switch (tok.ty) {
        .String => self.advanceAlloc(.{ .String = tok.text }),
        .Float => self.advanceAlloc(.{ .Float = tok.text }),
        .Int => self.advanceAlloc(.{ .Int = tok.text }),
        .True => self.advanceAlloc(.{ .Bool = true }),
        .False => self.advanceAlloc(.{ .Bool = false }),
        .Ident => self.parse_ident(),
        .Fn => self.parse_fnDecl(),
        .If => self.parse_if(),
        .Comptime => node: {
            self.advance();
            break :node self.alloc(.{ .Comptime = self.parse_expr() });
        },
        .Interface => self.parse_interface(),
        .Struct => self.parse_struct(),
        .LParen => node: {
            self.advance();

            const expr = self.parse_expr();
            if (self.advanceIf(.RParen)) |_| {
                break :node expr;
            }

            break :node self.alloc(.{ .Error = .{
                .msg = "'(' Needs a closing ')'",
                .token = tok,
            } });
        },
        .LBrace => self.parse_scope(),
        else => return self.advanceAlloc(.{ .Error = .{
            .msg = "Unable to parse value",
            .token = tok,
        } }),
    };

    if (self.advanceIf(.LParen)) |_| {
        var args = std.ArrayList(AST.NodeRef).init(self.allocator);

        while (true) {
            const arg = self.parse_expr();

            args.append(arg) catch |err| {
                @panic(@errorName(err));
            };

            if (self.advanceIf(.Colon) == null) break;
        }

        if (self.advanceIf(.RParen) == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Function call requires closing brace",
                .token = tok,
            } });
        }

        return self.alloc(.{ .FnCall = .{
            .@"fn" = value,
            .args = args,
        } });
    }

    return value;
}

fn expression(
    self: *Self,
    lhs: AST.NodeRef,
    min_precedence: i32,
) AST.NodeRef {
    var final = lhs;
    var current = self.getCurrent();

    while (Token.precendence(current) >= min_precedence) {
        const op = current;
        self.advance();

        var rhs = self.parse_value();

        current = self.getCurrent();

        while (Token.precendence(current) > Token.precendence(op)) {
            rhs = self.expression(
                rhs,
                Token.precendence(op) + @as(i32, if (Token.precendence(current) > Token.precendence(op))
                    1
                else
                    0),
            );
        }

        final = self.alloc(.{ .Binary = .{
            .lhs = final,
            .Op = op,
            .rhs = rhs,
        } });
    }
    return final;
}

fn parse_expr(self: *Self) AST.NodeRef {
    return self.expression(self.parse_value(), 0);
}

fn parse_field(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    const ident = self.parse_ident();
    const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_|
        self.parse_value()
    else
        null;

    const default: ?AST.NodeRef = if (self.advanceIf(.Equal)) |_| node: {
        break :node self.parse_value();
    } else node: {
        if (@"type" == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Field requires a default value or type signature",
                .token = start,
            } });
        }

        break :node null;
    };

    return self.alloc(.{ .Field = .{
        .ident = ident,
        .type = @"type",
        .default = default,
    } });
}

fn parse_interface(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Interface)) |_| {
        if (self.advanceIf(.LBrace) == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Interface expects braces",
                .token = start,
            } });
        }

        var fields = std.ArrayList(AST.NodeRef).init(self.allocator);

        while (self.advanceIf(.RBrace) == null) {
            fields.append(self.parse_field()) catch |err| {
                @panic(@errorName(err));
            };

            if (self.advanceIf(.Semicolon) == null) {
                return self.alloc(.{ .Error = .{
                    .msg = "Each field must end with a semicolon",
                    .token = start,
                } });
            }
        }

        return self.alloc(.{ .Interface = .{
            .fields = fields,
        } });
    }

    return self.alloc(.{ .Error = .{
        .msg = "Unable to parse interface",
        .token = start,
    } });
}

fn parse_struct(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Struct)) |_| {
        if (self.advanceIf(.LBrace) == null) {
            return self.alloc(.{ .Error = .{
                .msg = "Interface expects braces",
                .token = start,
            } });
        }

        var fields = std.ArrayList(AST.NodeRef).init(self.allocator);

        while (self.advanceIf(.RBrace) == null) {
            fields.append(self.parse_field()) catch |err| {
                @panic(@errorName(err));
            };

            if (self.advanceIf(.Semicolon) == null) {
                return self.alloc(.{ .Error = .{
                    .msg = "Each field must end with a semicolon",
                    .token = start,
                } });
            }
        }

        return self.alloc(.{ .Interface = .{
            .fields = fields,
        } });
    }

    return self.alloc(.{ .Error = .{
        .msg = "Unable to parse struct",
        .token = start,
    } });
}

fn parse_if(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.If)) |_| {
        const cond = self.parse_expr();
        if (self.advanceIf(.Then)) |_| {
            const @"true" = self.parse_expr();
            if (self.advanceIf(.Else)) |_| {
                const @"false" = self.parse_expr();

                return self.alloc(.{ .If = .{
                    .cond = cond,
                    .false = @"false",
                    .true = @"true",
                } });
            }
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse if statement",
        .token = start,
    } });
}

fn parse_const(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Const)) |_| {
        const ident = self.parse_ident();
        const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_|
            self.parse_expr()
        else
            null;

        if (self.advanceIf(.Equal)) |_| {
            const value = self.parse_expr();

            return self.alloc(.{ .ConstDecl = .{
                .ident = ident,
                .type = @"type",
                .value = value,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse const statement",
        .token = start,
    } });
}

fn parse_var(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.Var)) |_| {
        const ident = self.parse_ident();
        const @"type": ?AST.NodeRef = if (self.advanceIf(.Colon)) |_| ty: {
            break :ty self.parse_expr();
        } else null;

        if (self.advanceIf(.Equal)) |_| {
            const value = self.parse_expr();

            return self.alloc(.{ .VarDecl = .{
                .ident = ident,
                .type = @"type",
                .value = value,
            } });
        }
    }

    return self.alloc(.{ .Error = .{
        .msg = "Failed to parse var statement",
        .token = start,
    } });
}

fn parse_stmt(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    const stmt = switch (start.ty) {
        .Const => self.parse_const(),
        .Var => self.parse_var(),
        .Return => node: {
            self.advance();
            break :node self.alloc(.{ .Return = .{
                .value = self.parse_expr(),
            } });
        },
        else => self.parse_expr(),
    };

    if (self.advanceIf(.Semicolon)) |_| {
        return stmt;
    }

    return self.alloc(.{ .Error = .{
        .msg = "Statement must end with a semicolon",
        .token = start,
    } });
}

fn parse_scope(self: *Self) AST.NodeRef {
    const start = self.getCurrent();

    if (self.advanceIf(.LBrace) == null) {
        return self.alloc(.{ .Error = .{
            .msg = "Block needs opening brace",
            .token = start,
        } });
    }

    var list = std.ArrayList(AST.NodeRef).init(self.allocator);
    while (self.advanceIf(.RBrace) == null) {
        list.append(self.parse_stmt()) catch |err| {
            @panic(@errorName(err));
        };
    }

    return self.alloc(.{ .Scope = list });
}

fn parse_toplevel(self: *Self) AST.NodeRef {
    var list = std.ArrayList(AST.NodeRef).init(self.allocator);
    while (!self.eof()) {
        list.append(self.parse_stmt()) catch |err| {
            @panic(@errorName(err));
        };
    }

    return self.alloc(.{ .Scope = list });
}

pub fn run(self: *Self) void {
    self.ast.root = self.parse_toplevel();
}
