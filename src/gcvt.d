/// Wrapper for gcvt.
module gcvt;

string toString(const double value)
    @trusted pure nothrow
{
    immutable length = 40;

    auto buffer = new char[length];

    assert(buffer.length == length);

    gcvt(value, 10, buffer.ptr);

    import std.string : fromStringz;
    return fromStringz(buffer.ptr);
}

@safe pure nothrow unittest
{
    const x = 3.14.toString;
    assert(x == `3.14`);
}

extern(C) pragma(inline, false)
{
    pure nothrow @nogc:

    char *gcvt(double number, int ndigit, char *buf);
}
