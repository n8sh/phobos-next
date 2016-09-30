module bithashset;

enum Growable { no, yes }

@safe pure nothrow @nogc:

enum isBitHashable(T) = is(typeof(cast(size_t)T.init));

unittest
{
    static assert(isBitHashable!size_t);
    static assert(!isBitHashable!string);
}

/** Store presence of elements of type `E` in a set in the range `0 .. length`. */
struct BitHashSet(E, Growable growable = Growable.no)
    if (isBitHashable!E)
{
    /// Construct set to store at most `length` number of bits.
    this(size_t length) @trusted
    {
        _length = length;
        _bits = cast(Block*)calloc(blockCount, Block.sizeof);
    }

    ~this() @trusted
    {
        free(_bits);
    }

    @disable this(this);        // no copy ctor for now

    /// Returns: deep duplicate of `this`.
    typeof(this) dup() @trusted
    {
        typeof(this) copy;
        copy._length = _length;
        copy._bits = cast(Block*)malloc(blockCount * Block.sizeof);
        copy._bits[0 .. blockCount] = this._bits[0 .. blockCount];
        return copy;
    }

    import core.bitop : bts, btr, btc, bt;

    @property:

    /// Insert element `e`.
    void insert(E e) @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _length);
        bts(_bits, ix);
    }

    /// Remove element `e`.
    void remove(E e) @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _length);
        btr(_bits, ix);
    }

    /** Insert element `e` if it's present otherwise remove it.
        Returns: `true` if elements was zeroed, `false` otherwise.
     */
    bool complement(E e) @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _length);
        return btc(_bits, ix) != 0;
    }

    /// Check if element `e` is stored.
    bool contains(E e) const @trusted
    {
        const ix = cast(size_t)e;
        assert(ix < _length);
        return bt(_bits, ix) != 0;
    }

    /// ditto
    auto opBinaryRight(string op)(E e) const
        if (op == "in")
    {
        return contains(e);
    }

private:
    size_t length() const { return _length; }

    size_t blockCount() const
    {
        return length / Block.max + (length % Block.max ? 1 : 0);
    }

    alias Block = size_t;       ///< allocate block type
    size_t _length;             ///< number of bits stored
    Block* _bits;               ///< bits
}

unittest
{
    alias E = uint;

    auto w = BitHashSet!E();    // construct empty
    assert(w.length == 0);

    const length = 64;
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

/// qualify memory allocations
extern(C)
{
    void* malloc(size_t size);
    void* calloc(size_t nmemb, size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}
