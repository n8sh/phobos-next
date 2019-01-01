struct S(E)
{
    @safe pure nothrow @nogc:

    @property ref E x() return { return _x; }
    @property E* xptr() return { return &_x; }

    inout(E)[] opSlice() inout return @trusted { return _small[]; }

    private E _x;
    private E[2] _small;
}

///
@safe pure unittest
{
    alias E = int;
    static assert(!__traits(compiles, { ref E escape_x() { S s; return s.x; }}));
    static assert(!__traits(compiles, { E* escape_xptr() { S s; return s.xptr; } }));
    E[] escape_opSlice()
    {
        S!E s;
        return s[];
    }
}
