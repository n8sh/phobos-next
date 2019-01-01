struct S
{
    @safe pure nothrow @nogc:
    private int _x;
    @property ref int x() return { return _x; }
    @property int* xptr() return { return &_x; }
}

///
@safe pure unittest
{
    static assert(!__traits(compiles, { ref int escape_x() { S s; return s.x; }}));
    // int* escape_xptr() { S s; return s.xptr; }
}
