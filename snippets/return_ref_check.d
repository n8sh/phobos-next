struct S(E)
{
    @safe pure nothrow @nogc:

    @property ref E front() return
    {
        return _small.data[0];
    }
    @property E* frontPtr() return
    {
        return &_small.data[0];
    }

    inout(E)[] opSlice() inout return @trusted
    {
        if (isLarge)
        {
            return _large;
        }
        else
        {
            return _small.data[0 .. _small.length/2];
        }
    }

    ref inout(E) opIndex(size_t index) inout return @trusted
    {
        if (isLarge)
        {
            return _large[index];
        }
        else
        {
            return _small.data.ptr[index];
        }
    }

    static if (is(E == char))
    {
        alias toString = opSlice;
    }

    @property bool isLarge() const @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _large.length & 1; // first bit discriminates small from large
    }

private:
    enum smallCapacity = 15;
    alias Large = E[];
    version(LittleEndian) // see: http://forum.dlang.org/posting/zifyahfohbwavwkwbgmw
    {
        struct Small
        {
            /* TODO only first 4 bits are needed to represent a length between
             * 0-15, use other 4 bits */
            ubyte length = 0;
            E[smallCapacity] data = [0,0,0,0,0,
                                     0,0,0,0,0,
                                     0,0,0,0,0]; // explicit init needed for `__traits(isZeroInit)` to be true.
        }
    }

    struct Raw                  // same memory layout as `immutable(E)[]`
    {
        size_t length = 0;      // can be bit-fiddled without GC allocation
        E* ptr = null;
    }

    union
    {
        Raw raw;
        E[] _large;
        Small _small;
    }
}

///
@safe pure unittest
{
    alias E = char;
    static assert(!__traits(compiles, { ref E escape_x() { S s; return s.front; }}));
    static assert(!__traits(compiles, { E* escape_xptr() { S s; return s.frontPtr; } }));
    static assert(!__traits(compiles, { ref E escape_opIndex() { S s; return s[0]; }}));
    static assert(!__traits(compiles, { E[] escape_opSlice() { S!E s; return s[]; }  }));
    static assert(!__traits(compiles, { E[] escape_toString() { S!E s; return s.toString; } }));
}
