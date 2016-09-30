module bithashset;

/** Store precense of elements of type `E` in a set in the range `0 .. length`. */
struct BitHashSet(E)
{
    @trusted pure nothrow @nogc:

    @disable this();            // need length so no default construction

    this(size_t length)
    {
        _length = length;
        _bits = cast(Block*)calloc(length / Block.max +
                                   (length % Block.max ? 1 : 0),
                                   Block.sizeof);
    }

    ~this()
    {
        free(_bits);
    }


    import core.bitop : bts, btr, btc, bt;

    @property:

    /// Insert element `e`.
    void insert(E e) { const ix = cast(size_t)e; assert(ix < _length); bts(_bits, ix); }

    /// Remove element `e`.
    void remove(E e) { const ix = cast(size_t)e; assert(ix < _length); btr(_bits, ix); }

    /// Insert element `e` if it's present otherwise remove it.
    bool complement(E e) { const ix = cast(size_t)e; assert(ix < _length); return btc(_bits, ix) != 0; }

    /// Check if element `e` is contained in the set.
    bool contains(E e) const { const ix = cast(size_t)e; assert(ix < _length); return bt(_bits, ix) != 0; }

private:
    alias Block = size_t;       // allocate block type
    size_t _length;
    Block* _bits;
}

@safe pure nothrow @nogc unittest
{
    const length = 64;
    auto set = BitHashSet!uint(length);

    foreach (ix; 0 .. length)
    {
        assert(!set.contains(ix));
        set.insert(ix);
        assert(set.contains(ix));
        set.complement(ix);
        assert(!set.contains(ix));
        set.complement(ix);
        assert(set.contains(ix));
    }

    foreach (ix; 0 .. length)
    {
        assert(set.contains(ix));
        set.remove(ix);
        assert(!set.contains(ix));
    }
}

extern(C) pure nothrow @system @nogc
{
    void* malloc(size_t size);
    void* calloc(size_t nmemb, size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}
