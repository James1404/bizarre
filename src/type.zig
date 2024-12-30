const std = @import("std");

const TypeRef = *Type;
const Type = union(enum) {
    Int: struct { size: i32, signed: bool },
    Float: struct { size: i32 },
    String,
    Bool,

    Array: struct { inner: TypeRef },
    Fn: struct {
        args: []TypeRef,
        ret: TypeRef,
    },

    Pointer: struct { inner: TypeRef },
};
