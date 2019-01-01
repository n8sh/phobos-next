struct S(E)
{
    @safe pure nothrow @nogc:

    @property ref E x() return { return _x; }
    @property E* xptr() return { return &_x; }

    inout(E)[] opSlice() inout return @trusted { return (&_x)[0 .. 1]; }

    private E _x;
}

///
@safe pure unittest
{
    alias E = int;
    static assert(!__traits(compiles, { ref E escape_x() { S s; return s.x; }}));
    E[] escape_opSlice()
    {
        S!E s;
        return s[];
    }

    // E* escape_xptr() { S s; return s.xptr; }
}
