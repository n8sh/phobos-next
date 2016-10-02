module bithashset;

enum Growable { no, yes }

enum isBitHashable(T) = is(typeof(cast(size_t)T.init)); // TODO use `isIntegral` instead?

unittest
{
    static assert(isBitHashable!size_t);
    static assert(!isBitHashable!string);
}

// version = show;
// version(show)
// import dbgio : dln;

/** Store presence of elements of type `E` in a set in the range `0 .. length`. */
struct BitHashSet(E, Growable growable = Growable.no)
    if (isBitHashable!E)
{
    import qcmeman;
    import core.bitop : bts, btr, btc, bt;

    @safe pure nothrow @nogc pragma(inline):

    /// Construct set to store at most `length` number of bits.
    this(size_t length) @trusted
    {
        _length = length;
        _blocksPtr = cast(Block*)calloc(blockCount, Block.sizeof);
    }

    ~this() @trusted
    {
        free(_blocksPtr);
    }

    @disable this(this);        // no copy ctor for now

    /// Returns: deep duplicate of `this`.
    typeof(this) dup() @trusted
    {
        typeof(this) copy;
        copy._length = _length;
        copy._blocksPtr = cast(Block*)malloc(blockCount * Block.sizeof);
        copy._blocksPtr[0 .. blockCount] = this._blocksPtr[0 .. blockCount];
        return copy;
    }


    @property:

    static if (growable == Growable.yes)
    {
        /// Expand to `newLength`.
        void assureCapacity(size_t newLength) @trusted
        {
            if (length < newLength)
            {
                const oldBlockCount = blockCount;
                import std.math : nextPow2;
                this._length = newLength.nextPow2;
                _blocksPtr = cast(Block*)realloc(_blocksPtr, blockCount * Block.sizeof);
                _blocksPtr[oldBlockCount .. blockCount] = 0;
            }
        }
    }

    /// Insert element `e`.
    void insert(E e) @trusted
    {
        const ix = cast(size_t)e;
        static if (growable == Growable.yes) { assureCapacity(ix + 1); _length = ix + 1; } else { assert(ix < _length); }
        bts(_blocksPtr, ix);
    }

    /// Remove element `e`.
    void remove(E e) @trusted
    {
        const ix = cast(size_t)e;
        static if (growable == Growable.yes) { assureCapacity(ix + 1); _length = ix + 1; } else { assert(ix < _length); }
        btr(_blocksPtr, ix);
    }

    /** Insert element `e` if it's present otherwise remove it.
        Returns: `true` if elements was zeroed, `false` otherwise.
     */
    bool complement(E e) @trusted
    {
        const ix = cast(size_t)e;
        static if (growable == Growable.yes) { assureCapacity(ix + 1); _length = ix + 1; } else { assert(ix < _length); }
        return btc(_blocksPtr, ix) != 0;
    }

    /// Check if element `e` is stored/contained.
    bool contains(E e) @trusted const
    {
        const ix = cast(size_t)e;
        return ix < length && bt(_blocksPtr, ix) != 0;
    }

    /// ditto
    auto opBinaryRight(string op)(E e) const
        if (op == "in")
    {
        return contains(e);
    }

private:
    @property size_t length() const { return _length; }

    @property size_t blockCount() const
    {
        return length / Block.sizeof + (length % Block.sizeof ? 1 : 0);
    }

    alias Block = size_t;       ///< allocate block type
    size_t _length;             ///< number of bits stored
    Block* _blocksPtr;          ///< pointer to blocks of bits
}

///
@safe pure nothrow @nogc unittest
{
    alias E = uint;

    const w = BitHashSet!(E, Growable.no)();
    assert(w.length == 0);

    const length = 2^^6;
    auto x = BitHashSet!E(2*length);
    const y = x.dup;
    assert(y.length == 2*length);

    foreach (ix; 0 .. length)
    {
        assert(!x.contains(ix));
        assert(ix !in x);

        x.insert(ix);
        assert(x.contains(ix));
        assert(ix in x);

        assert(x.complement(ix));
        assert(!x.contains(ix));
        assert(ix !in x);

        assert(!x.complement(ix));
        assert(x.contains(ix));
        assert(ix in x);
    }

    auto z = x.dup;
    foreach (ix; 0 .. length)
    {
        assert(z.contains(ix));
        assert(ix in z);
    }

    foreach (ix; 0 .. length)
    {
        assert(x.contains(ix));
        assert(ix in x);
    }

    foreach (ix; 0 .. length)
    {
        assert(x.contains(ix));
        x.remove(ix);
        assert(!x.contains(ix));
    }
}

///
@safe pure nothrow @nogc unittest
{
    alias E = uint;

    auto x = BitHashSet!(E, Growable.yes)();
    assert(x.length == 0);

    const length = 2^^16;
    foreach (ix; 0 .. length)
    {
        assert(!x.contains(ix));
        assert(ix !in x);

        x.insert(ix);
        assert(x.contains(ix));
        assert(ix in x);

        assert(x.complement(ix));
        assert(!x.contains(ix));
        assert(ix !in x);

        assert(!x.complement(ix));
        assert(x.contains(ix));
        assert(ix in x);
    }
}

/// test `RefCounted` storage
nothrow @nogc unittest          // TODO @safe pure when https://github.com/dlang/phobos/pull/4692/files has been merged
{
    import std.typecons : RefCounted;
    alias E = int;
    RefCounted!(BitHashSet!(E, Growable.yes)) x;

    assert(x.length == 0);

    x.insert(0);
    assert(x.length == 1);

    auto y = x;

    foreach (const e; 1 .. 1000)
    {
        x.insert(e);
        assert(x.length == e + 1);
        assert(y.length == e + 1);
    }
}
