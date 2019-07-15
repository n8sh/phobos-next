/**
 * Static bit array container for internal usage.
 */
module simple_static_bitarray;

static private alias Block = size_t;

struct StaticBitArray(uint length_)
{
    import core.bitop : bt, bts, btr;

@safe pure nothrow @nogc:

    /** Number of bits. */
    enum length = length_;

    /** Number of bits per `Block`. */
    enum bitsPerBlock = 8*Block.sizeof;

    /** Number of `Block`s. */
    enum blockCount = (length_ + (bitsPerBlock-1)) / bitsPerBlock;

    /** Reset all bits (to zero). */
    void reset() nothrow
    {
        pragma(inline, true);
        _blocks[] = 0;          // TODO is this the fastest way?
    }

    /** Gets the $(D idx)'th bit. */
    bool opIndex(size_t idx) const @trusted nothrow
    in
    {
        assert(idx < length);     // TODO nothrow or not?
    }
    body
    {
        pragma(inline, true);
        return cast(bool)bt(_blocks.ptr, idx);
    }

    /** Sets the $(D idx)'th bit. */
    bool opIndexAssign(bool b, size_t idx) @trusted nothrow
    in
    {
        assert(idx < length);     // TODO nothrow or not?
    }
    body
    {
        pragma(inline, true);
        if (b)
        {
            bts(_blocks.ptr, cast(size_t)idx);
        }
        else
        {
            btr(_blocks.ptr, cast(size_t)idx);
        }
        return b;
    }

    /** Find index of first cleared (zero) bit or `typeof(return).max` if no bit set.
     *
     * Optimized for zeros-sparsity.
     */
    size_t indexOfFirstZero()() const
    {
        import bitarray_algorithm;
        enum bool blockAlignedLength = length_ % (8*Block.sizeof) == 0;
        return bitarray_algorithm.indexOfFirstZero!(const(Block)[blockCount],
                                                    blockAlignedLength)(_blocks, length);
    }

    /** Find index of first set (one) bit or `typeof(return).max` if no bit set.
     *
     * Optimized for ones-sparsity.
     */
    size_t indexOfFirstOne()() const
    {
        import bitarray_algorithm;
        enum bool blockAlignedLength = length_ % (8*Block.sizeof) == 0;
        return bitarray_algorithm.indexOfFirstOne!(const(Block)[blockCount],
                                                   blockAlignedLength)(_blocks, length);
    }

    private Block[blockCount] _blocks;
}

@trusted pure unittest
{
    enum blockCount = 2;
    enum length = blockCount * 8*Block.sizeof - 1; // 2 blocks minus one

    StaticBitArray!(length) x;
    static assert(x.blockCount == blockCount);
    
    assertThrown!AssertError(x[length] = false);

    x[length - 1] = true;
    assert(x[length - 1]);
    
    x[length - 1] = false;
    assert(!x[length - 1]);
}

version(unittest)
{
    import std.exception: assertThrown;
    import core.exception : AssertError;
}
