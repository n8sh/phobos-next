module basic_bitarray;

/** Array of bits. */
struct BitArray(alias Allocator = null)
{
    import qcmeman : malloc, calloc, realloc, free;

    pragma(inline, true)
    @safe pure nothrow @nogc:

    /** Construct with `length` number of bits. */
    this(size_t length) @trusted
    {
        _blockCount = ((length / blockBits) + // number of whole blocks
                       (length % blockBits ? 1 : 0)); // remained block
        _ptr = cast(Block*)malloc(blockBits*_blockCount);
        _length = length;
    }

    ~this() @trusted
    {
        free(_ptr);
    }

    /// Check if empty.
    bool empty() const { return _length == 0; }

    /// Get length.
    @property size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    /// Get capacity in number of bits.
    pragma(inline, true)
    @property size_t capacity() const { return blockBits*_blockCount; }

    @disable this(this);

private:
    alias Block = size_t;
    enum blockBits = 8*Block.sizeof;

    Block* _ptr;
    size_t _blockCount;
    size_t _length;             // TODO remove this
}

version = show;

@safe pure nothrow @nogc unittest
{
    const bitCount = 100;

    auto a = BitArray!(null)(bitCount);

    assert(a.length == bitCount);
    assert(a.capacity == 2*a.blockBits);
}

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio : dln;
}
