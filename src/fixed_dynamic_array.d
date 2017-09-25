module fixed_dynamic_array;

/** Dynamically allocated (heap) array with fixed length.
 */
private struct FixedDynamicArray(E)
{
    import qcmeman : malloc, free;

pragma(inline, true):

    this(size_t length)
    {
        _length = length;
        _storage = cast(E*)malloc(length * E.sizeof);
    }

    ~this()
    {
        free(_storage);
    }

    @disable this(this);        // no copying

    scope inout(E)[] opSlice() inout @trusted return
    {
        return _storage[0 .. _length];
    }

private:
    size_t _length;
    E* _storage;
}
