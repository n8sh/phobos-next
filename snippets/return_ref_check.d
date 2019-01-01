struct S(E)
{
    @safe pure nothrow @nogc:

    @property ref E x() return { return _small[0]; }
    @property E* xptr() return { return &_small[0]; }

    inout(E)[] opSlice() inout return @trusted
    {
        return _small[0 .. 16];
    }

    static if (is(E == char))
    {
        alias toString = opSlice;
    }

private:
    union
    {
        E[16] _small;
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
