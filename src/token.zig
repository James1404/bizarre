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

    Const,
    Var,

    And,
    Or,
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

    .{ "const", .Const },
    .{ "var", .Var },

    .{ "and", .And },
    .{ "or", .Or },
});

pub const List = std.ArrayList(@This());