module basic_array;

private struct BasicArray(E, alias Allocator = null) // null means means to qcmeman functions
{
    import std.traits : Unqual;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;

    /// Mutable element type.
    private alias MutableE = Unqual!E;

    /// Type of `this`.
    private alias This = typeof(this);

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    pragma(inline)
    static This withLength(size_t initialLength) @trusted nothrow
    {
        debug typeof(return) that;
        else typeof(return) that = void;

        return that;
    }

private:

    BasicStore!(E) _store;
}

private struct BasicStore(E,
                          bool useGCAllocation = false)
{
    import std.traits : Unqual;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;

    /// Mutable element type.
    private alias MutableE = Unqual!E;

    this(size_t initialCapacity, size_t initialLength, bool zero)
    {
        setCapacity(initialCapacity);
        this.capacity = initialCapacity;
        this.ptr = allocate(initialCapacity, zero);
        this.length = initialLength;
    }

pragma(inline, true):

    void setCapacity(size_t newCapacity)
    {
        capacity = newCapacity;
    }

    MutableE* _mptr() const @trusted
    {
        return cast(typeof(return))ptr;
    }

    /** Allocate heap regionwith `newCapacity` number of elements of type `E`.
        If `zero` is `true` they will be zero-initialized.
    */
    private static MutableE* allocate(size_t newCapacity, bool zero = false)
        pure nothrow
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

    static if (useGCAllocation)
    {
        E* ptr; // GC-allocated store pointer. See also: http://forum.dlang.org/post/iubialncuhahhxsfvbbg@forum.dlang.org
    }
    else
    {
        @nogc E* ptr;   // non-GC-allocated store pointer
    }

    size_t capacity;            // store capacity
    size_t length;              // store length
}

template shouldAddGCRange(T)
{
    import std.traits : hasIndirections;
    enum shouldAddGCRange = hasIndirections!T; // TODO unless all pointers members are tagged as @nogc (as in `BasicArray` and `BasicStore`)
}


@safe pure nothrow @nogc unittest
{
    BasicArray!int x;
}
