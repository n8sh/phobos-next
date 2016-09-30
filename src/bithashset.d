module bithashset;

enum Growable { no, yes }

/** Store presence of elements of type `E` in a set in the range `0 .. length`. */
struct BitHashSet(E, Growable growable = Growable.no)
{
    @safe pure nothrow @nogc:

    /// Construct to store `length` number of bits.
    this(size_t length) @trusted
    {
        _bitCount = length;
        _bits = cast(Block*)calloc(blockCount, Block.sizeof);
    }

    ~this() @trusted
    {
        free(_bits);
    }

    @disable this(this);        // no copy ctor for now

    /// Return deep duplicate of `this`.
    typeof(this) dup() @trusted
    {
        auto copy = typeof(this)(_bitCount);
        copy._bits[0 .. blockCount] = this._bits[0 .. blockCount];
        return copy;
    }

    import core.bitop : bts, btr, btc, bt;

    @property:

    /// Insert element `e`.
    void insert(E e) @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _bitCount);
        bts(_bits, ix);
    }

    /// Remove element `e`.
    void remove(E e) @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _bitCount);
        btr(_bits, ix);
    }

    /// Insert element `e` if it's present otherwise remove it.
    bool complement(E e) @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _bitCount);
        return btc(_bits, ix) != 0;
    }

    /// Check if element `e` is contained in the set.
    bool contains(E e) const @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _bitCount);
        return bt(_bits, ix) != 0;
    }

    /// ditto
    auto opBinaryRight(string op)(E e) const
        if (op == "in")
    {
        return contains(e);
    }

    private size_t length() const { return _bitCount; }

    private size_t blockCount() const
    {
        return length / Block.max + (length % Block.max ? 1 : 0);
    }

private:
    alias Block = size_t;       ///< allocate block type
    size_t _bitCount;           ///< number of bits stored
    Block* _bits;               ///< bits
}

@safe pure nothrow @nogc unittest
{
    const length = 64;
    auto x = BitHashSet!uint(2*length);
    const y = x.dup;

    foreach (ix; 0 .. length)
    {
        assert(!x.contains(ix));
        assert(ix !in x);

        x.insert(ix);
        assert(x.contains(ix));
        assert(ix in x);

        x.complement(ix);
        assert(!x.contains(ix));
        assert(ix !in x);

        x.complement(ix);
        assert(x.contains(ix));
        assert(ix in x);
    }

    auto z = x.dup;
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

/// qualify memory allocations
extern(C) pure nothrow @system @nogc
{
    void* malloc(size_t size);
    void* calloc(size_t nmemb, size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}
