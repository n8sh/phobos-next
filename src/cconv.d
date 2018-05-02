/// High-level wrappers for C-conversion functions.
module cconv;

string toString(const double value,
                uint digitCount)
    @trusted pure nothrow
{
    immutable length = 3 + digitCount; // digits plus + sign + dot + null

    auto buffer = new char[length];

    gcvt(value, digitCount, buffer.ptr);

    import std.string : fromStringz;
    return fromStringz(buffer.ptr);
}

///
@safe pure nothrow unittest
{
    assert(0.0.toString(1) == `0`);
    assert(0.1.toString(0) == `0.1`);
    assert(0.1.toString(1) == `0.1`);

    assert((-1.0).toString(1) == `-1`);
    assert((-1.0).toString(2) == `-1`);

    assert(3.14.toString(3) == `3.14`);
    assert(3.141.toString(1) == `3`);
    assert(3.141.toString(2) == `3.1`);
    assert(3.141.toString(3) == `3.14`);
    assert(3.141.toString(4) == `3.141`);
    assert(3.141.toString(5) == `3.141`);

    assert(1234567.123456789123456789.toString(20) == `1234567.1234567892`);
}

private extern(C) pragma(inline, false)
{
    pure nothrow @nogc:
    char *gcvt(double, int, char*);
}
