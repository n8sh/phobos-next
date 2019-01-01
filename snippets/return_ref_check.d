struct S(E)
{
    @safe pure nothrow @nogc:

    @property ref E x() return { return _small; }
    @property E* xptr() return { return &_small; }

    inout(E)[] opSlice() inout return @trusted { return (&_small)[0 .. 1]; }

    static if (is(E == char))
    {
        inout(E)[] toString() inout return @trusted { return opSlice(); }
    }

private:
    union
    {
        E _small;
    }
}

///
@safe pure unittest
{
    alias E = char;
    static assert(!__traits(compiles, { ref E escape_x() { S s; return s.x; }}));
    static assert(!__traits(compiles, { E* escape_xptr() { S s; return s.xptr; } }));
    E[] escape_opSlice()
    {
        S!E s;
        return s[];
    }
    E[] escape_toString()
    {
        S!E s;
        return s.toString;
    }
}
