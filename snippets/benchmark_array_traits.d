import std.traits : isDynamicArray;

enum E : string
{
    a = "a",
    b = "b",
}

@safe pure unittest
{
    static assert(isDynamicArray!E);
    static assert(!is(E == T[], T));
    // static assert(__traits(isDynamicArray, E));
}
