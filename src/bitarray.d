/** Bitarray.
 */
module bitarray;

@safe:

/** Array of bits.
 *
 * Like `std.bitmanip.BitArray` but @safe pure nothrow @nogc.
 *
 * Set `blockAlignedLength` to true if `this.length` is always a multiple of
 * `Block.size`.
 *
 * TODO use `Flag` instead, or wrap in `BlockAlignedBitArray` where this class
 * is made private _BitArray and alias BitArray = _BitArray!(true).
 */
struct BitArray(bool blockAlignedLength = false,
                alias Allocator = null) // TODO use Allocator
{
    import core.memory : pureMalloc, pureCalloc, pureFree;
    import core.bitop : bt, bts, btr;
    import bitarray_algorithm;

@safe pure nothrow @nogc:

    /** Construct with `length` number of zero bits. */
    static typeof(this) withLength(size_t length) @trusted
    {
        typeof(return) that;    // TODO = void
        static if (blockAlignedLength)
        {
            assert(length % bitsPerBlock == 0,
                   "Parameter length is not a multiple of " ~ bitsPerBlock.stringof);
            that._blockCount = length / bitsPerBlock; // number of whole blocks
        }
        else
        {
            that._blockCount = ((length / bitsPerBlock) + // number of whole blocks
                                (length % bitsPerBlock ? 1 : 0)); // remained block
            that._length = length;
        }
        that._blockPtr = cast(Block*)pureCalloc(bitsPerBlock, that._blockCount); // TODO use `Allocator`
        return that;
    }

    /** Construct with `length` number of zero bits stored in `blocks`. */
    private static typeof(this) withLengthAndBlocks(size_t length,
                                                    const scope Block[] blocks)
    {
        return typeof(return)(length, blocks);
    }

    /** Helper constructor. */
    private this(size_t length,
                 const scope Block[] blocks) @trusted
    {
        _blockCount = blocks.length;
        _blockPtr = cast(Block*)pureMalloc(bitsPerBlock * _blockCount); // TODO use `Allocator`
        _blocks[] = blocks; // copy block array
        static if (!blockAlignedLength)
        {
            _length = length;
        }
    }

    /// Destroy.
    ~this()
    {
        release();
    }

    /// Explicit copying (duplicate).
    typeof(this) dup()
    {
        return typeof(this).withLengthAndBlocks(_length, _blocks);
    }

    /// Empty.
    void reset()
    {
        release();
        resetInternalData();
    }
    alias clear = reset;

    /// Release internal store.
    private void release() @trusted
    {
        pureFree(_blockPtr);
    }

    /// Reset internal data.
    private void resetInternalData()
    {
        _blockPtr = null;
        _blockCount = 0;
        static if (!blockAlignedLength)
        {
            _length = 0;
        }
    }

    /// Check if empty.
    bool empty() const { return _length == 0; }

    /// Get length.
    @property size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    /// Get capacity in number of bits.
    @property size_t capacity() const { return bitsPerBlock*_blockCount; }

    /** Get the `i`'th bit. */
    bool opIndex(size_t i) const @trusted
    {
        pragma(inline, true);
        assert(i < length);        // TODO nothrow or not?
        return cast(bool)bt(_blockPtr, i);
    }

    /** Set the `i`'th bit to `value`. */
    bool opIndexAssign(bool value, size_t i) @trusted
    {
        pragma(inline, true);
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

    /** Set all bits to `value` via slice assignment syntax. */
    ref typeof(this) opSliceAssign(bool value)
    {
        if (value)
        {
            one();
        }
        else
        {
            zero();
        }
        return this;
    }

    /** Set all bits to zero. */
    private void zero()
    {
        foreach (ref block; _blocks)
        {
            block = Block.min;
        }
    }

    /** Set all bits to one. */
    private void one()
    {
        foreach (ref block; _blocks)
        {
            block = Block.max;
        }
    }

    /** Get number of (one) bits set. */
    size_t countOnes()() const  // template-lazy
    {
        return bitarray_algorithm.countOnes(_blocks, length);
    }

    /** Get number of (zero) bits unset. */
    size_t countZeros()() const  // template-lazy
    {
        return length - countOnes;
    }

    /** Find index of first set (one) bit or `length` if no bit set.
     *
     * Optimized for ones-sparsity.
     */
    size_t indexOfFirstOne()() const
    {
        return bitarray_algorithm.indexOfFirstOne(_blocks, length);
    }

    /** Find index of first cleared (zero) bit or `length` if no bit cleared.
     *
     * Optimized for zeros-sparsity.
     */
    size_t indexOfFirstZero()() const
    {
        return bitarray_algorithm.indexOfFirstZero(_blocks, length);
    }

    /** Equality, operators == and !=. */
    bool opEquals(in ref typeof(this) rhs) const @trusted
    {
        static if (!blockAlignedLength)
        {
            if (length != rhs.length)
            {
                return false;
            }
        }
        if (_fullBlocks != rhs._fullBlocks)
        {
            return false;
        }
        static if (!blockAlignedLength)
        {
            const restBitCount = length % bitsPerBlock;
            if (restBitCount)
            {
                return _restBlock == rhs._restBlock;
            }
        }
        return true;
    }

    /** Only explicit copying via `.dup` for now. */
    @disable this(this);

private:

    /** Get blocks. */
    inout(Block)[] _blocks() inout @trusted
    {
        return _blockPtr[0 .. _blockCount];
    }

    static if (blockAlignedLength)
    {
        inout(Block)[] _fullBlocks() inout @trusted
        {
            return _blocks;
        }
    }
    else
    {
        inout(Block)[] _fullBlocks() inout @trusted
        {
            pragma(inline, true);
            const fullBlockCount = length / bitsPerBlock;
            return _blocks.ptr[0 .. fullBlockCount];
        }

        Block _restBlock() const @trusted
        {
            const restBitCount = length % bitsPerBlock;
            return _blocks[$-1] & ((1UL << restBitCount) - 1);
        }
    }

    alias Block = size_t;
    enum bitsPerBlock = 8*Block.sizeof; /// Number of bits per `Block`.

    /** Number of Block's allocated at `_blockPtr`. */
    size_t _blockCount;

    static if (is(Allocator == std.experimental.allocator.gc_allocator.GCAllocator))
    {
        Block* _blockPtr;       // GC-allocated store pointer
    }
    else
    {
        import gc_traits : NoGc;
        @NoGc Block* _blockPtr; // non-GC-allocated store pointer
    }

    static if (blockAlignedLength)
    {
        @property size_t _length() const
        {
            return bitsPerBlock * _blockCount;
        }
    }
    else
    {
        size_t _length;
    }
}

/// Test indexing and element assignment.
@safe pure nothrow @nogc unittest
{
    const bitCount = 100;

    auto a = BitArray!().withLength(bitCount);

    assert(a.length == bitCount);
    assert(a.capacity == 2*a.bitsPerBlock);
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

/// Test `countOnes` and `countZeros`.
@safe pure nothrow @nogc unittest
{
    foreach (const n; 1 .. 3*BitArray!().bitsPerBlock)
    {
        auto a = BitArray!().withLength(n);

        // set bits forwards
        foreach (const i; 0 .. n)
        {
            assert(a.countOnes == i);
            assert(a.countZeros == n - i);
            a[i] = true;
            assert(a.countOnes == i + 1);
            assert(a.countZeros == n - (i + 1));
        }

        assert(a.countOnes == n);
        assert(a.countZeros == 0);

        auto b = a.dup;
        assert(b.countOnes == n);
        assert(b.countZeros == 0);

        assert(a == b);

        // clear `a` bits forwards
        foreach (const i; 0 .. n)
        {
            assert(a.countOnes == n - i);
            assert(a.countZeros == i);
            a[i] = false;
            assert(a.countOnes == n - (i + 1));
            assert(a.countZeros == i + 1);
        }

        b[] = false;
        assert(a == b);

        // set bits backwards
        foreach (const i; 0 .. n)
        {
            assert(a.countOnes == i);
            a[n-1 - i] = true;
            assert(a.countOnes == i + 1);
        }

        b[] = true;
        assert(a == b);
    }
}

/// Test emptying (resetting) via `.clear` and explicit copying with `.dup`.
@safe pure nothrow @nogc unittest
{
    const n = 5;

    auto a = BitArray!().withLength(n);
    assert(a.length == n);

    a.reset();
    assert(a.length == 0);

    a = BitArray!().withLength(n);
    assert(a.length == n);

    auto b = a.dup;
    assert(b.length == n);

    a.reset();
    assert(a.length == 0);
}

/// Test `indexOfFirstOne` for single set ones.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    assert(a.length == n);
    assert(a.indexOfFirstOne == n); // miss

    a[0] = true;
    assert(a.indexOfFirstOne == 0);
    a[] = false;

    a[2] = true;
    assert(a.indexOfFirstOne == 2);
    a[] = false;

    a[n/2-1] = true;
    assert(a.indexOfFirstOne == n/2-1);
    a[] = false;

    a[n/2] = true;
    assert(a.indexOfFirstOne == n/2);
    a[] = false;

    a[n/2+1] = true;
    assert(a.indexOfFirstOne == n/2+1);
    a[] = false;

    a[n-1] = true;
    assert(a.indexOfFirstOne == n-1);
    a[] = false;

    assert(a.indexOfFirstOne == n); // miss
}

/// Test `indexOfFirstOne` for multi set ones.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);

    a[0] = true;
    a[BitArray!().bitsPerBlock/2] = true;
    a[BitArray!().bitsPerBlock - 1] = true;
    assert(a.indexOfFirstOne == 0);
}

/// Test `indexOfFirstZero` for single set zeros.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    a[] = true;

    assert(a.length == n);
    assert(a.indexOfFirstZero == n); // miss

    a[0] = false;
    assert(a.indexOfFirstZero == 0);
    a[0] = true;

    a[2] = false;
    assert(a.indexOfFirstZero == 2);
    a[2] = true;

    a[n/2-1] = false;
    assert(a.indexOfFirstZero == n/2-1);
    a[n/2-1] = true;

    a[n/2] = false;
    assert(a.indexOfFirstZero == n/2);
    a[n/2] = true;

    a[n/2+1] = false;
    assert(a.indexOfFirstZero == n/2+1);
    a[n/2+1] = true;

    a[n-1] = false;
    assert(a.indexOfFirstZero == n-1);
    a[n-1] = true;

    assert(a.indexOfFirstZero == n); // miss
}

/// Test `indexOfFirstZero` for multi set zeros.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    a[] = true;

    a[0] = false;
    a[BitArray!().bitsPerBlock/2] = false;
    a[BitArray!().bitsPerBlock - 1] = false;
    assert(a.indexOfFirstZero == 0);
}

/// Test `indexOfFirstOne` for multi set ones.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    a[] = false;

    a[0] = true;
    a[BitArray!().bitsPerBlock/2] = true;
    a[BitArray!().bitsPerBlock - 1] = true;
    assert(a.indexOfFirstOne == 0);
}

///
@trusted pure unittest
{
    assertThrown!AssertError(BitArray!(true).withLength(1));
}

version(unittest)
{
    import std.exception: assertThrown;
    import core.exception : AssertError;
}
