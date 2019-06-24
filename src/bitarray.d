/** Bitarray.
 */
module bitarray;

@safe:

/** Array of bits.
 *
 * Like `std.bitmanip.BitArray` but @safe pure nothrow @nogc.
 */
struct BitArray(alias Allocator = null) // TODO use Allocator
{
    import core.memory : pureMalloc, pureCalloc, pureFree;
    import core.bitop : bt, bts, btr;
    import bitarray_algorithm;

    @safe pure nothrow @nogc:

    /** Construct with `length` number of zero bits. */
    static typeof(this) withLength(size_t length) @trusted
    {
        typeof(return) that;
        that._blockCount = ((length / bitsPerBlock) + // number of whole blocks
                            (length % bitsPerBlock ? 1 : 0)); // remained block
        that._blockPtr = cast(Block*)pureCalloc(bitsPerBlock, that._blockCount);
        that._length = length;
        return that;
    }

    /** Construct with `length` number of zero bits stored in `blocks`. */
    static typeof(this) withLengthAndBlocks(size_t length,
                                            Block[] blocks) @trusted
    {
        typeof(return) that;
        that._blockCount = blocks.length;
        that._blockPtr = cast(Block*)pureMalloc(bitsPerBlock * that._blockCount);
        that._blocks[] = blocks;
        that._length = length;
        return that;
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
        _length = 0;
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
        return bitarray_algorithm.countOnes(_blocks);
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

    /** Find index of last set (one) bit or `length` if no bit set.
     *
     * Optimized for ones-sparsity.
     */
    size_t indexOfLastOne()() const
    {
        return bitarray_algorithm.indexOfLastOne(_blocks, length);
    }

    /** Find index of first cleared (zero) bit or `length` if no bit cleared.
     *
     * Optimized for zeros-sparsity.
     */
    size_t indexOfFirstZero()() const
    {
        return bitarray_algorithm.indexOfFirstZero(_blocks, length);
    }

    /** Find index of last cleared (zero) bit or `length` if no bit cleared.
     *
     * Optimized for zeros-sparsity.
     */
    size_t indexOfLastZero()() const
    {
        return bitarray_algorithm.indexOfLastZero(_blocks, length);
    }

    /** Equality, operators == and !=. */
    bool opEquals(in ref typeof(this) rhs) const
    {
        return _blocks == rhs._blocks;
    }

    /** Only explicit copying via `.dup` for now. */
    @disable this(this);

private:

    /** Get blocks. */
    inout(Block)[] _blocks() inout @trusted
    {
        return _blockPtr[0 .. _blockCount];
    }

    alias Block = size_t;
    enum bitsPerBlock = 8*Block.sizeof;

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

    size_t _length;
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
    const n = 5;

    auto a = BitArray!().withLength(n);
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

/// Test `indexOfFirstOne` and `indexOfLastOne` for single set ones.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    assert(a.length == n);
    assert(a.indexOfFirstOne == n); // miss
    assert(a.indexOfLastOne == n); // miss

    a[0] = true;
    assert(a.indexOfFirstOne == 0);
    assert(a.indexOfLastOne == 0);
    a[] = false;

    a[2] = true;
    assert(a.indexOfFirstOne == 2);
    assert(a.indexOfLastOne == 2);
    a[] = false;

    a[n/2-1] = true;
    assert(a.indexOfFirstOne == n/2-1);
    assert(a.indexOfLastOne == n/2-1);
    a[] = false;

    a[n/2] = true;
    assert(a.indexOfFirstOne == n/2);
    assert(a.indexOfLastOne == n/2);
    a[] = false;

    a[n/2+1] = true;
    assert(a.indexOfFirstOne == n/2+1);
    assert(a.indexOfLastOne == n/2+1);
    a[] = false;

    a[n-1] = true;
    assert(a.indexOfFirstOne == n-1);
    assert(a.indexOfLastOne == n-1);
    a[] = false;

    assert(a.indexOfFirstOne == n); // miss
    assert(a.indexOfLastOne == n);  // miss
}

/// Test `indexOfFirstOne` and `indexOfLastOne` for multi set ones.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);

    a[0] = true;
    a[BitArray!().bitsPerBlock/2] = true;
    a[BitArray!().bitsPerBlock - 1] = true;
    assert(a.indexOfFirstOne == 0);
    assert(a.indexOfLastOne == BitArray!().bitsPerBlock - 1);
}

/// Test `indexOfFirstZero` and `indexOfLastZero` for single set zeros.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    a[] = true;

    assert(a.length == n);
    assert(a.indexOfFirstZero == n);          // miss
    assert(a.indexOfLastZero == n);           // miss

    a[0] = false;
    assert(a.indexOfFirstZero == 0);
    assert(a.indexOfLastZero == 0);
    a[0] = true;

    a[2] = false;
    assert(a.indexOfFirstZero == 2);
    assert(a.indexOfLastZero == 2);
    a[2] = true;

    a[n/2-1] = false;
    assert(a.indexOfFirstZero == n/2-1);
    assert(a.indexOfLastZero == n/2-1);
    a[n/2-1] = true;

    a[n/2] = false;
    assert(a.indexOfFirstZero == n/2);
    assert(a.indexOfLastZero == n/2);
    a[n/2] = true;

    a[n/2+1] = false;
    assert(a.indexOfFirstZero == n/2+1);
    assert(a.indexOfLastZero == n/2+1);
    a[n/2+1] = true;

    a[n-1] = false;
    assert(a.indexOfFirstZero == n-1);
    assert(a.indexOfLastZero == n-1);
    a[n-1] = true;

    assert(a.indexOfFirstZero == n); // miss
    assert(a.indexOfLastZero == n);  // miss
}

/// Test `indexOfFirstZero` and `indexOfLastZero` for multi set zeros.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    a[] = true;

    a[0] = false;
    a[BitArray!().bitsPerBlock/2] = false;
    a[BitArray!().bitsPerBlock - 1] = false;
    assert(a.indexOfFirstZero == 0);
    assert(a.indexOfLastZero == BitArray!().bitsPerBlock - 1);
}

/// Test `indexOfFirstOne` and `indexOfLastOne` for multi set ones.
@safe pure nothrow @nogc unittest
{
    enum n = 2 * BitArray!().bitsPerBlock;

    auto a = BitArray!().withLength(n);
    a[] = false;

    a[0] = true;
    a[BitArray!().bitsPerBlock/2] = true;
    a[BitArray!().bitsPerBlock - 1] = true;
    assert(a.indexOfFirstOne == 0);
    assert(a.indexOfLastOne == BitArray!().bitsPerBlock - 1);
}

version(unittest)
{
    import array_help : s;
}
