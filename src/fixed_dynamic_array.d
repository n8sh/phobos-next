module fixed_dynamic_array;

/** Dynamically allocated (heap) array with fixed length.
 */
private struct FixedDynamicArray(E)
{
    import qcmeman : pureMalloc = malloc, pureFree = free;

pragma(inline):

    /// Make and return uninitialized array of `length`.
    static typeof(this) makeUninitialized(size_t length) @system
    {
        return typeof(return)(length);
    }

pragma(inline, true):

    /// Construct uninitialized array of `length`.
    private this(size_t length) @system
    {
        _length = length;
        _storage = cast(E*)pureMalloc(length * E.sizeof);
    }

    /// Destruct.
    ~this() @trusted
    {
        pureFree(_storage);
    }

    // disable copying
    @disable this(this);

    /// Get element at index `i`.
    scope ref inout(E) opIndex(size_t i) inout @system return
    {
        return _storage[i];
    }

    /// Slice support.
    scope inout(E)[] opSlice(size_t i, size_t j) inout @system return
    {
        return _storage[i .. j];
    }
    /// ditto
    scope inout(E)[] opSlice() inout @system return
    {
        return _storage[0 .. _length];
    }

private:
    size_t _length;
    E* _storage;
}

@system pure nothrow @nogc unittest
{
    auto x = FixedDynamicArray!(int).makeUninitialized(7);
    x[0] = 11;
    assert(x[0] == 11);
}
