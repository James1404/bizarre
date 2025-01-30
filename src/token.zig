const std = @import("std");

pub const Ty = enum {
    None,

    // Values
    String,
    Int,
    Float,
    Ident,

    // Symbols
    Dot,
    Comma,
    Colon,
    Semicolon,

    Plus,
    Asterix,
    Minus,
    Slash,
    Percent,

    Less,
    Greater,
    GreaterEq,
    LessEq,

    Equal,

    NotEqual,
    EqualEqual,

    Not,

    LParen,
    RParen,

    LBracket,
    RBracket,

    LBrace,
    RBrace,

    Arrow,

    // Keywords

    True,
    False,

    If,
    Then,
    Else,

    For,
    While,

    Return,

    Const,
    Var,

    And,
    Or,

    Fn,
    Comptime,
    Extern,

    Interface,
    Struct,
    Distinct,

    Defer,

    As,
};

ty: Ty,

text: []const u8,

pos: usize,
line: usize,
offset: usize,

pub const Keywords = std.StaticStringMap(Ty).initComptime(.{
    .{ "true", .True },
    .{ "false", .False },

    .{ "if", .If },
    .{ "then", .Then },
    .{ "else", .Else },

    .{ "for", .For },
    .{ "while", .While },

    .{ "return", .Return },

    .{ "const", .Const },
    .{ "var", .Var },

    .{ "and", .And },
    .{ "or", .Or },

    .{ "fn", .Fn },
    .{ "comptime", .Comptime },
    .{ "extern", .Extern },

    .{ "interface", .Interface },
    .{ "struct", .Struct },
    .{ "distinct", .Distinct },

    .{ "defer", .Defer },

    .{ "as", .As },
});

pub fn precendence(tok: @This()) i32 {
    return switch (tok.ty) {
        .Plus => 1,
        .Minus => 2,
        .Asterix => 3,
        .Slash => 4,

        .Less,
        .LessEq,
        .Greater,
        .GreaterEq,
        .EqualEqual,
        .NotEqual,
        => 4,

        else => -1,
    };
}
pub const Precendence = std.AutoHashMap(Ty, i32);

pub const List = std.ArrayList(@This());
