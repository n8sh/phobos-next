struct S(E)
{
    @safe pure nothrow @nogc:

    @property ref E x() return { return _x; }
    @property E* xptr() return { return &_x; }

    scope E[] opSlice() return { xptr[0 .. 1]; }

    private E _x;
}

///
@safe pure unittest
{
    static assert(!__traits(compiles, { ref int escape_x() { S s; return s.x; }}));
    // int* escape_xptr() { S s; return s.xptr; }
}
