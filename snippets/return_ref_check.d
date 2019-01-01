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
    enum smallCapacity = 15;
    version(LittleEndian) // see: http://forum.dlang.org/posting/zifyahfohbwavwkwbgmw
    {
        struct Small
        {
            /* TODO only first 4 bits are needed to represent a length between
             * 0-15, use other 4 bits */
            ubyte length = 0;
            immutable(E)[smallCapacity] data = [0,0,0,0,0,
                                                0,0,0,0,0,
                                                0,0,0,0,0]; // explicit init needed for `__traits(isZeroInit)` to be true.
        }
    }
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
