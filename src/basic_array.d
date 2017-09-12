module basic_array;

private struct BasicArray(E, alias Allocator = null) // null means means to qcmeman functions
{
    import std.traits : Unqual;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;

    /// Mutable element type.
    private alias MutableE = Unqual!E;

    /// Type of `this`.
    private alias This = typeof(this);

    enum useGCAllocation = false; // TODO set if Allocator is GCAllocator

    this(size_t initialCapacity, size_t initialLength, bool zero = true) @trusted
    {
        this._capacity = initialCapacity;
        this._ptr = allocate(initialCapacity, zero);
        this._length = initialLength;
    }

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    pragma(inline)
    static This withLength(size_t initialLength)
    {
        return This(initialLength, initialLength, true);
    }

    /** Allocate heap regionwith `newCapacity` number of elements of type `E`.
        If `zero` is `true` they will be zero-initialized.
    */
    private static MutableE* allocate(size_t newCapacity, bool zero = false)
    {
        typeof(return) ptr = null;
        static if (useGCAllocation)
        {
            if (zero) { ptr = cast(typeof(return))GC.calloc(newCapacity, E.sizeof); }
            else      { ptr = cast(typeof(return))GC.malloc(newCapacity * E.sizeof); }
        }
        else                    // @nogc
        {
            if (zero) { ptr = cast(typeof(return))calloc(newCapacity, E.sizeof); }
            else      { ptr = cast(typeof(return))malloc(newCapacity * E.sizeof); }
            assert(ptr, "Allocation failed");
        }
        static if (shouldAddGCRange!E)
        {
            gc_addRange(ptr, newCapacity * E.sizeof);
        }
        return ptr;
    }

pragma(inline, true):

    /// Check if empty.
    bool empty() const { return _length == 0; }

    /// Get length.
    size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    /// Mutable pointer.
    MutableE* _mptr() const @trusted
    {
        return cast(typeof(return))_ptr;
    }

private:
    static if (useGCAllocation)
    {
        E* _ptr; // GC-allocated store pointer. See also: http://forum.dlang.org/post/iubialncuhahhxsfvbbg@forum.dlang.org
    }
    else
    {
        @nogc E* _ptr;   // non-GC-allocated store pointer
    }

    size_t _capacity;            // store capacity
    size_t _length;              // store length
}

private struct BasicStore(E, bool useGCAllocation = false)
{
}

template shouldAddGCRange(T)
{
    import std.traits : hasIndirections;
    enum shouldAddGCRange = hasIndirections!T; // TODO unless all pointers members are tagged as @nogc (as in `BasicArray` and `BasicStore`)
}


@safe pure nothrow @nogc unittest
{
    BasicArray!int a;
    assert(a.empty);

    const a10 = BasicArray!int.withLength(10);
    assert(!a10.empty);
    assert(a10.length == 10);
}
