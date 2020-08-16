/// See_Also: https://github.com/dlang/dmd/pull/9014#issuecomment-674451700
/// See_Also: https://github.com/dlang/phobos/pull/7574

import std.traits : isStaticArray, isDynamicArray;

enum C : char[1]
{
    a = "a",
    b = "b",
}

static assert(isStaticArray!C);
static assert(!is(D == T[1], T));
static assert(__traits(isStaticArray, C));

enum D : string
{
    a = "a",
    b = "b",
}

static assert(isDynamicArray!D);
static assert(!is(D == T[], T));

import std.meta : AliasSeq;

static foreach (T; AliasSeq!(char, wchar, dchar,
                             byte, ubyte,
                             short, ushort,
                             int, uint,
                             long, ulong,
                             float, double, real,
                             cfloat, cdouble, creal,
                             ifloat, idouble, ireal))
{
    static assert(__traits(isDynamicArray, T[]));
    static assert(__traits(isDynamicArray, const(T)[]));
    static assert(__traits(isDynamicArray, inout(T)[]));
    static assert(__traits(isDynamicArray, immutable(T)[]));
}
