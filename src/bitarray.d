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
                   "Parameter `length` is not a multiple `Block` bit size " ~ bitsPerBlock.stringof);
            that._blockCount = length / bitsPerBlock; // number of whole blocks
        }
        else
        {
            that._blockCount = (length + bitsPerBlock-1) / bitsPerBlock;
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

    /** Clear all bits (to zero). */
    private void zero()
    {
        foreach (ref block; _blocks)
        {
            block = Block.min;
        }
    }

    /** Set all bits (to one). */
    private void one()
    {
        foreach (ref block; _blocks)
        {
            block = Block.max;
        }
    }

    /** Get number of bits set (to one). */
    size_t countOnes()() const  // template-lazy
    {
        return bitarray_algorithm.countOnes!(const(Block)[], blockAlignedLength)(_blocks, length);
    }

    /** Get number of bits cleared (to zero). */
    size_t countZeros()() const  // template-lazy
    {
        return length - countOnes;
    }

    /** Find index of first set (one) bit or `length` if no bit set.
     *
     * Optimized for ones-sparsity.
     */
    size_t indexOfFirstOne()() const // template-lazy
    {
        return bitarray_algorithm.indexOfFirstOne!(const(Block)[], blockAlignedLength)(_blocks, length);
    }

    /** Find index of first cleared (zero) bit or `length` if no bit cleared.
     *
     * Optimized for zeros-sparsity.
     */
    size_t indexOfFirstZero()() const // template-lazy
    {
        return bitarray_algorithm.indexOfFirstZero!(const(Block)[], blockAlignedLength)(_blocks, length);
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

/// Test `_blockCount` and `_fullBlocks.length`.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);
        assert(BA.withLength(0)._blockCount == 0);
        assert(BA.withLength(1)._blockCount == 1);

        assert(BA.withLength(1*BA.bitsPerBlock - 1)._blockCount == 1);
        assert(BA.withLength(1*BA.bitsPerBlock - 1)._fullBlocks.length == 0);

        assert(BA.withLength(1*BA.bitsPerBlock + 0)._blockCount == 1);
        assert(BA.withLength(1*BA.bitsPerBlock + 0)._fullBlocks.length == 1);

        assert(BA.withLength(1*BA.bitsPerBlock + 1)._blockCount == 2);
        assert(BA.withLength(1*BA.bitsPerBlock + 1)._fullBlocks.length == 1);

        assert(BA.withLength(2*BA.bitsPerBlock - 1)._blockCount == 2);
        assert(BA.withLength(2*BA.bitsPerBlock - 1)._fullBlocks.length == 1);

        assert(BA.withLength(2*BA.bitsPerBlock + 0)._blockCount == 2);
        assert(BA.withLength(2*BA.bitsPerBlock + 0)._fullBlocks.length == 2);

        assert(BA.withLength(2*BA.bitsPerBlock + 1)._blockCount == 3);
        assert(BA.withLength(2*BA.bitsPerBlock + 1)._fullBlocks.length == 2);
    }
    test!(false)();
}

/// Test indexing and element assignment.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)(size_t length)
    {
        alias BA = BitArray!(blockAlignedLength);

        auto a = BA.withLength(length);

        assert(a.length == length);
        foreach (const i; 0 .. length)
        {
            assert(!a[i]);
        }

        a[0] = true;
        assert(a[0]);
        foreach (const i; 1 .. length)
        {
            assert(!a[i]);
        }

        assert(!a[1]);
        a[1] = true;
        assert(a[1]);
        a[1] = false;
        assert(!a[1]);
    }
    test!(false)(100);
    test!(true)(64);
}

/// Test `countOnes` and `countZeros`.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);

        foreach (const n; 1 .. 5*BA.bitsPerBlock)
        {
            static if (blockAlignedLength)
            {
                if (n % BA.bitsPerBlock != 0) // if block aligned length
                {
                    continue;
                }
            }

            auto a = BA.withLength(n);

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
    test!(false)();
    test!(true)();
}

/// Test emptying (resetting) via `.clear` and explicit copying with `.dup`.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);

        static if (blockAlignedLength)
        {
            const n = 5 * BA.bitsPerBlock;
        }
        else
        {
            const n = 5 * BA.bitsPerBlock + 1;
        }
        auto a = BA.withLength(n);

        assert(a.length == n);

        a.reset();
        assert(a.length == 0);

        a = BA.withLength(n);
        assert(a.length == n);

        auto b = a.dup;
        assert(b.length == n);

        a.reset();
        assert(a.length == 0);
    }
    test!(false)();
    test!(true)();
}

/// Test `indexOfFirstOne` for single set ones.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);

        static if (blockAlignedLength)
        {
            const n = 2 * BA.bitsPerBlock;
        }
        else
        {
            const n = 2 * BA.bitsPerBlock + 1;
        }
        auto a = BA.withLength(n);

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
    test!(false)();
    test!(true)();
}

/// Test `indexOfFirstOne` for multi set ones.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);

        static if (blockAlignedLength)
        {
            const n = 2 * BA.bitsPerBlock;
        }
        else
        {
            const n = 2 * BA.bitsPerBlock + 1;
        }
        auto a = BA.withLength(n);

        a[0] = true;
        a[BA.bitsPerBlock/2] = true;
        a[BA.bitsPerBlock - 1] = true;
        assert(a.indexOfFirstOne == 0);
    }
    test!(false)();
    test!(true)();
}

/// Test `indexOfFirstZero` for single set zeros.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);

        static if (blockAlignedLength)
        {
            const n = 2 * BA.bitsPerBlock;
        }
        else
        {
            const n = 2 * BA.bitsPerBlock + 1;
        }
        auto a = BA.withLength(n);

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
    test!(false)();
    test!(true)();
}

@safe pure nothrow @nogc unittest
{
    alias BA = BitArray!(false);
    static assert(BitArray!(false).sizeof == 3*BA.Block.sizeof); // one extra word for `length`
    static assert(BitArray!(true).sizeof == 2*BA.Block.sizeof);
}

/// Test `indexOfFirstZero` for multi set zeros.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);

        static if (blockAlignedLength)
        {
            const n = 2 * BA.bitsPerBlock;
        }
        else
        {
            const n = 2 * BA.bitsPerBlock + 1;
        }
        auto a = BA.withLength(n);

        a[] = true;

        a[0] = false;
        a[BA.bitsPerBlock/2] = false;
        a[BA.bitsPerBlock - 1] = false;
        assert(a.indexOfFirstZero == 0);
    }
    test!(false)();
    test!(true)();
}

/// Test `indexOfFirstOne` for multi set ones.
@safe pure nothrow @nogc unittest
{
    static void test(bool blockAlignedLength)()
    {
        alias BA = BitArray!(blockAlignedLength);

        static if (blockAlignedLength)
        {
            const n = 2 * BA.bitsPerBlock;
        }
        else
        {
            const n = 2 * BA.bitsPerBlock + 1;
        }
        auto a = BA.withLength(n);

        a[] = false;

        a[0] = true;
        a[BA.bitsPerBlock/2] = true;
        a[BA.bitsPerBlock - 1] = true;
        assert(a.indexOfFirstOne == 0);
    }
    test!(false)();
    test!(true)();
}

///
@trusted pure unittest
{
    assertThrown!AssertError(BitArray!(true).withLength(1));
}

///
@safe pure nothrow @nogc unittest
{
}

version(unittest)
{
    import std.exception: assertThrown;
    import core.exception : AssertError;
}
