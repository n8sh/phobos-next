module bithashset;

/** Store precense of elements of type `E` in a set in the range `0 .. length`. */
struct BitHashSet(E)
{
    @disable this();            // need length so no default construction

    this(size_t length) { _bits.length = length; }

    @property:

    void insert(E ix) { assert(ix < _bits.length); _bits[ix] = true; }
    void remove(E ix) { assert(ix < _bits.length); _bits[ix] = false; }
    bool contains(E ix) const { assert(ix < _bits.length); return _bits[ix]; }

private:
    import std.bitmanip : BitArray;
    BitArray _bits;
}

unittest
{
    const length = 64;
    auto set = BitHashSet!uint(length);
    foreach (ix; 0 .. length)
    {
        assert(!set.contains(ix));
        set.insert(ix);
        assert(set.contains(ix));
    }
}
