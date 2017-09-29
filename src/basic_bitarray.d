module basic_bitarray;

/** Array of bits. */
struct BitArray(alias Allocator = null)
{
    @safe pure nothrow @nogc:

    /** Construct with `length` number of bits. */
    this(size_t length)
    {
        immutable blockCount = ((length / blockBits) + // number of whole blocks
                                (length % blockBits ? 1 : 0)); // remained block
        _store = Store.withLength(blockCount);
        _length = length;
    }

    /// Check if empty.
    pragma(inline, true)
    bool empty() const { return _length == 0; }

    /// Get length.
    pragma(inline, true)
    @property size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    @disable this(this);

private:
    import basic_copyable_array : Array = CopyableArray;

    alias Block = size_t;
    enum blockBits = 8*Block.sizeof;
    alias Store = Array!(Block, Allocator);

    Store _store;
    size_t _length;
}

version = show;

@safe pure nothrow @nogc unittest
{
    const bitCount = 100;
    auto a = BitArray!(null)(bitCount);
    assert(a.length == bitCount);
    assert(a._store.length == 2);
}

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio : dln;
}
