/**
 * Static bit array container for internal usage.
 */
module simple_static_bitarray;

struct StaticBitArray(uint length_, Block = size_t)
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

    private Block[blockCount] _blocks;
}

@safe pure nothrow @nogc:

///
unittest
{
    alias Block = size_t;
    enum blockCount = 2;
    enum length = blockCount * 8*Block.sizeof - 1;
    StaticBitArray!(length) x;
    static assert(x.blockCount == blockCount);
}
