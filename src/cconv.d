/// High-level wrappers for C-conversion functions.
module cconv;

string toString(const double value,
                uint digitCount)
    @trusted pure nothrow
{
    immutable length = 40;

    auto buffer = new char[length];

    assert(buffer.length == length);

    gcvt(value, digitCount, buffer.ptr);

    import std.string : fromStringz;
    return fromStringz(buffer.ptr);
}

///
@safe pure nothrow unittest
{
    assert(3.14.toString(3) == `3.14`);
    assert(3.141.toString(3) == `3.14`);
    assert(3.141.toString(4) == `3.141`);
    assert(3.141.toString(5) == `3.141`);
    assert(1234567.123456789123456789.toString(20) == `1234567.1234567892`);
}

extern(C) pragma(inline, false)
{
    pure nothrow @nogc:
    char *gcvt(double number, int ndigit, char *buf);
}
