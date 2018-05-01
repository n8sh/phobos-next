/// Wrapper for gcvt.
module gcvt;

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

@safe pure nothrow unittest
{
    // import dbgio;
    // dln(1234567.123456789123456789.toString(100));
    assert(1234567.123456789123456789.toString(20) == `1234567.1234567892`);
}

extern(C) pragma(inline, false)
{
    pure nothrow @nogc:

    char *gcvt(double number, int ndigit, char *buf);
}
