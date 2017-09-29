module basic_bitarray;

/** Array of bits. */
struct BitArray(alias Allocator = null)
{
    @safe pure nothrow @nogc:

    /** Construct with `length` number of bits. */
    this(size_t length)
    {
        immutable blockCount = length / blockBits + (length % blockBits ? 1 : 0);
        _store = Store.withLength(blockCount);
    }

private:
    import basic_copyable_array : Array = CopyableArray;
    alias Block = size_t;
    enum blockBits = 8*Block.sizeof;
    alias Store = Array!(Block, Allocator);
    Store _store;
}

version = show;

@safe pure nothrow @nogc unittest
{
    const bitCount = 100;
    auto a = BitArray!(null)(bitCount);
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
