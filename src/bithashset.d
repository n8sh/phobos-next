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


    import core.bitop : bts, btr, bt;

    @property:

    /// Insert element `e`.
    void insert(E e) { assert(e < _length); bts(_bits, cast(size_t)e); }

    /// Remove element `e`.
    void remove(E e) { assert(e < _length); btr(_bits, cast(size_t)e); }

    /// Check if element `e` is contained in the set.
    bool contains(E e) const { assert(e < _length); return bt(_bits, cast(size_t)e) != 0; }

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
