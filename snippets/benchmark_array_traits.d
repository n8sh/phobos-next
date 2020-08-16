//// See_Also: https://github.com/dlang/phobos/pull/7574

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
// static assert(__traits(isDynamicArray, D));
