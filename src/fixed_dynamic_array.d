module fixed_dynamic_array;

@safe:

/** Dynamically allocated (heap) array with fixed length.
 */
struct FixedDynamicArray(T)
{
@safe:
    import qcmeman : pureMalloc = malloc, pureCalloc = calloc, pureFree = free;
    import container_traits : mustAddGCRange;

pragma(inline, true):

    /** Make and return uninitialized array of `length`.
     *
     * Unlike `@trusted pureMalloc` this must be `@system` because the return
     * value of this factory function can be accessed in @safe code.
     */
    pragma(inline)              // DMD cannot inline
    static typeof(this) makeUninitializedOfLength(size_t length) @system
    {
        return typeof(return)(Store(length, cast(T*)pureMalloc(length * T.sizeof)));
    }

    pragma(inline)              // DMD cannot inline
    static typeof(this) withLength(size_t length) @system
    {
        return typeof(return)(Store(length, cast(T*)pureCalloc(length, T.sizeof)));
    }

    /// Construct from `store`.
    private this(Store store)
    {
        _store = store;
    }

    /// Construct uninitialized array of `length`.
    private this(size_t length) @system
    {
        _store.length = length;
        _store.ptr = cast(T*)pureMalloc(length * T.sizeof);
    }

    /// Destruct.
    ~this() @trusted
    {
        pureFree(_store.ptr);
    }

    // disable copying
    @disable this(this);

    /// Get element at index `i`.
    scope ref inout(T) opIndex(size_t i) inout @system return
    {
        return _store.ptr[i];
    }

    /// Slice support.
    scope inout(T)[] opSlice(size_t i, size_t j) inout @system return
    {
        return _store.ptr[i .. j];
    }
    /// ditto
    scope inout(T)[] opSlice() inout @system return
    {
        return _store.ptr[0 .. _store.length];
    }

    /// Slice assignment support.
    scope T[] opSliceAssign(U)(U value) return
    {
        return _store.ptr[0 .. _store.length] = value;
    }
    /// ditto
    scope T[] opSliceAssign(U)(U value, size_t i, size_t j) return
    {
        return _store.ptr[i .. j] = value;
    }

private:
    static struct Store
    {
        size_t length;
        import container_traits : NoGc;
        @NoGc T* ptr;           // non-GC-allocated store pointer
    }
    Store _store;
}

@system pure nothrow @nogc unittest
{
    auto x = FixedDynamicArray!(int).makeUninitializedOfLength(7);
    x[0] = 11;
    assert(x[0] == 11);

    auto y = FixedDynamicArray!(int).withLength(3);
    y[0] = 11;
    assert(y[] == [11, 0, 0].s);
}

version(unittest)
{
    import array_help : s;
}
