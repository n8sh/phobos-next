module basic_bitarray;

/** Array of bits.
    Like `std.bitmanip.BitArray` but @safe pure nothrow @nogc.
 */
struct BitArray(alias Allocator = null)
{
    import qcmeman : malloc, calloc, realloc, free;
    import core.bitop : bt, bts, btr;

    pragma(inline, true)
    @safe pure nothrow @nogc:

    /** Construct with `length` number of zero bits. */
    pragma(inline)              // TODO gcc cannot inline this
    static typeof(this) withLength(size_t length) @trusted
    {
        typeof(return) that = void;
        that._blockCount = ((length / blockBits) + // number of whole blocks
                            (length % blockBits ? 1 : 0)); // remained block
        that._ptr = cast(Block*)calloc(blockBits, that._blockCount);
        that._length = length;
        return that;
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
    @property size_t capacity() const { return blockBits*_blockCount; }

    /** Gets the $(D i)'th bit in the $(D BitArrayN). */
    bool opIndex(size_t i) const @trusted
    {
        assert(i < length);        // TODO nothrow or not?
        return cast(bool)bt(_ptr, i);
    }

    /** Set the `i`'th bit to `value`. */
    void opIndexAssign(bool value, size_t i) @trusted
    {
        bts(_ptr, i);
    }

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

    auto a = BitArray!(null).withLength(bitCount);

    assert(a.length == bitCount);
    assert(a.capacity == 2*a.blockBits);
    foreach (const i; 0 .. bitCount)
    {
        assert(!a[i]);
    }

    a[0] = true;
    assert(a[0]);
    foreach (const i; 1 .. bitCount)
    {
        assert(!a[i]);
    }

    assert(!a[1]);
    a[1] = true;
    assert(a[1]);
}

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio : dln;
}
