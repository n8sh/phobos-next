module bitarray;

/** Array of bits.
    Like `std.bitmanip.BitArray` but @safe pure nothrow @nogc.
 */
struct BitArray(alias Allocator = null) // TODO use Allocator
{
    import qcmeman : malloc, calloc, realloc, free;
    import core.bitop : bt, bts, btr;

    pragma(inline, true)
    @safe pure nothrow @nogc:

    /** Construct with `length` number of zero bits. */
    pragma(inline)              // TODO gcc cannot inline this
    static typeof(this) withLength(size_t length) @trusted
    {
        typeof(return) that;
        that._blockCount = ((length / blockBits) + // number of whole blocks
                            (length % blockBits ? 1 : 0)); // remained block
        that._blockPtr = cast(Block*)calloc(blockBits, that._blockCount);
        that._length = length;
        return that;
    }

    /** Construct with `length` number of zero bits stored in `blocks`. */
    pragma(inline)
    static typeof(this) withLengthAndBlocks(size_t length,
                                            Block[] blocks) @trusted
    {
        typeof(return) that;
        that._blockCount = blocks.length;
        that._blockPtr = cast(Block*)malloc(blockBits * that._blockCount);
        that._blocks[] = blocks;
        that._length = length;
        return that;
    }

    ~this() @trusted
    {
        free(_blockPtr);
    }

    /// Duplicate.
    typeof(this) dup()
    {
        return typeof(this).withLengthAndBlocks(_length, _blocks);
    }

    void clear() @trusted
    {
        free(_blockPtr);
        _blockPtr = null;
        _blockCount = 0;
        _length = 0;
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
        return cast(bool)bt(_blockPtr, i);
    }

    /** Set the `i`'th bit to `value`. */
    bool opIndexAssign(bool value, size_t i) @trusted
    {
        if (value)
        {
            bts(_blockPtr, i);
        }
        else
        {
            btr(_blockPtr, i);
        }
        return value;
    }

    /** Get number of bits set. */
    size_t countOnes() const    // TODO make free function
    {
        typeof(return) n = 0;
        foreach (const block; _blocks)
        {
            import core.bitop : popcnt;
            static if (block.sizeof == 1 ||
                       block.sizeof == 2 ||
                       block.sizeof == 4 ||
                       block.sizeof == 4)
            {
                // TODO do we need to force `uint`-overload of `popcnt`?
                n += cast(uint)block.popcnt;
            }
            else static if (block.sizeof == 8)
            {
                n += (cast(ulong)((cast(uint)(block)).popcnt) +
                      cast(ulong)((cast(uint)(block >> 32)).popcnt));
            }
            else
            {
                assert(false, "Unsupported Block size " ~ Block.sizeof.stringof);
            }
        }
        return typeof(return)(n);
    }

    /** Support for operators == and != for $(D BitArrayN). */
    bool opEquals(in ref typeof(this) rhs) const
    {
        return _blocks == rhs._blocks;
    }

    @disable this(this);

private:

    /** Get blocks. */
    inout(Block)[] _blocks() inout @trusted
    {
        return _blockPtr[0 .. _blockCount];
    }

    alias Block = size_t;
    enum blockBits = 8*Block.sizeof;

    // as slice
    size_t _blockCount;
    Block* _blockPtr;

    size_t _length;
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
    a[1] = false;
    assert(!a[1]);
}

@safe pure nothrow @nogc unittest
{
    const n = 5;

    auto a = BitArray!(null).withLength(n);
    foreach (const i; 0 .. n)
    {
        assert(a.countOnes == i);
        a[i] = true;
        assert(a.countOnes == i + 1);
    }
    assert(a.countOnes == n);

    auto b = a.dup;
    assert(b.countOnes == n);

    assert(a == b);
}

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio : dln;
}
