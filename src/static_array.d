module static_array;

/** Statically allocated `E`-array of fixed pre-allocated length.
    Similar to Rust's `fixedvec`: https://docs.rs/fixedvec/0.2.3/fixedvec/
*/
struct StaticArrayN(E, uint capacity)
{
    E[capacity] _store;         /// stored elements

    /// number of elements in `_store`
    static      if (capacity < ubyte.max + 1)  ubyte _length;
    else static if (capacity < ushort.max + 1) ushort _length;
    else static assert("Too large capacity " ~ capacity);

    alias ElementType = E;

    @safe pure nothrow @nogc:

    /** Construct with elements `ixs`. */
    this(Es...)(Es ixs)
        if (Es.length >= 1 &&
            Es.length <= capacity)
    {
        foreach (const i, const ix; ixs)
        {
            import std.algorithm.mutation : move;
            _store[i] = ix.move();
        }
        _length = ixs.length;
    }

    /** Check if empty. */
    bool empty() const @safe { return _length == 0; }

    /** Get length. */
    auto length() const { return _length; }
    alias opDollar = length;    /// ditto

    inout @trusted:

    /// Index operator.
    ref inout(E) opIndex(size_t i) // TODO DIP-1000 scope
    {
        assert(i < _length);
        return _store[i];
    }

    /// Returns: front element.
    ref inout(E) front()        // TODO DIP-1000 scope
    {
        assert(!empty);
        return _store[0];
    }

    /// Returns: back element.
    ref inout(E) back()         // TODO DIP-1000 scope
    {
        assert(!empty);
        return _store[_length - 1];
    }

    /// Slice operator.
    inout(E)[] opSlice()    // TODO DIP-1000 scope
    {
        return this.opSlice(0, _length);
    }
    /// ditto
    inout(E)[] opSlice(size_t i, size_t j) // TODO DIP-1000 scope
    {
        assert(i <= j);
        assert(j <= _length);
        return _store.ptr[i .. j]; // TODO DIP-1000 scope
    }
}

///
@safe pure unittest
{
    alias E = char;
    enum capacity = 3;

    alias A = StaticArrayN!(E, capacity);
    static assert(A.sizeof == E.sizeof*capacity + 1);

    auto ab = A('a', 'b');
    assert(ab[0] == 'a');
    assert(ab.front == 'a');
    assert(ab.back == 'b');
    assert(ab.length == 2);
    assert(ab[] == "ab");
    assert(ab[0 .. 1] == "a");

    const abc = A('a', 'b', 'c');
    assert(abc.front == 'a');
    assert(abc.back == 'c');
    assert(abc.length == 3);
    assert(abc[] == "abc");
    assert(ab[0 .. 2] == "ab");

    static assert(!__traits(compiles, { const abc = A('a', 'b', 'c', 'd'); }));
}
