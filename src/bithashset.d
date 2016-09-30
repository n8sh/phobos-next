module bithashset;

/** Store precense of elements of type `E` in a set in the range `0 .. length`. */
struct BitHashSet(E)
{
    @trusted pure nothrow @nogc:

    @disable this();            // need length so no default construction

    this(size_t length)
    {
        _length = length;
        _bits = cast(size_t*)calloc(length / size_t.max +
                                    (length % size_t.max ? 1 : 0), size_t.sizeof);
    }

    ~this()
    {
        free(_bits);
    }


    import core.bitop : bts, btr, bt;

    @property:
    void insert(E ix) { assert(ix < _length); bts(_bits, cast(size_t)ix); }
    void remove(E ix) { assert(ix < _length); btr(_bits, cast(size_t)ix); }
    bool contains(E ix) const { assert(ix < _length); return bt(_bits, cast(size_t)ix) != 0; }

private:
    size_t _length;
    size_t* _bits;
}

@safe pure unittest
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
