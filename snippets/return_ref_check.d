struct S(E)
{
    @safe pure nothrow @nogc:

    @property ref E x() return { return _small[0]; }
    @property E* xptr() return { return &_small[0]; }

    inout(E)[] opSlice() inout return @trusted
    {
        if (_largeFlag)
            return _large;
        else
            return _small[0 .. 16];
    }

    static if (is(E == char))
    {
        alias toString = opSlice;
    }

private:
    union
    {
        E[] _large;
        E[16] _small;
    }
    bool _largeFlag;
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
