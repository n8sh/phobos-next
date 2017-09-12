module basic_array;

private struct BasicArray(E, alias Allocator = null) // null means means to qcmeman functions
{
    import std.traits : Unqual, hasElaborateDestructor, hasIndirections, hasAliasing, isMutable;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;

    /// Mutable element type.
    private alias MutableE = Unqual!E;

    /// Type of `this`.
    private alias This = typeof(this);

    enum useGCAllocation = false; // TODO set if Allocator is GCAllocator

    @disable this(this);        // no copy construction

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    pragma(inline, true)
    static This withLength(size_t initialLength)
    {
        return This(initialLength, initialLength, true);
    }

    /// Returns: an array with initial capacity `initialCapacity`.
    pragma(inline, true)
    static This withCapacity(size_t initialCapacity)
    {
        return This(initialCapacity, 0);
    }

    private this(size_t initialCapacity, size_t initialLength = 0, bool zero = true) @trusted
    {
        assert(initialCapacity >= initialLength);
        this._capacity = initialCapacity;
        this._ptr = allocate(initialCapacity, zero);
        this._length = initialLength;
    }

    /// Destruct.
    pragma(inline, true)
    ~this()
    {
        release();
    }

    /// Empty.
    pragma(inline, true)
    void clear()
    {
        release();
        resetInternalData();
    }

    /// Destroy elements.
    static if (hasElaborateDestructor!E)
    {
        private void destroyElements() @trusted
        {
            foreach (const i; 0 .. _length)
            {
                .destroy(_mptr[i]);
            }
        }
    }

    /// Release internal store.
    private void release() @trusted
    {
        static if (hasElaborateDestructor!E)
        {
            destroyElements();
        }

        static if (shouldAddGCRange!E)
        {
            gc_removeRange(_ptr);
        }

        static if (useGCAllocation)
        {
            GC.free(_mptr);
        }
        else
        {
            free(_mptr);
        }
    }

    /// Reset internal data.
    private void resetInternalData()
    {
        _ptr = null;
        _length = 0;
        _capacity = 0;
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
    @property size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    /// Set length to `newLength`.
    @property void length(size_t newLength)
    {
        reserve(newLength);
        _length = newLength;
    }

    /// Get capacity.
    @property size_t capacity() const { return _capacity; }

    /// Index operator.
    ref inout(E) opIndex(size_t i) inout @trusted return scope
    {
        return slice()[i];
    }

    /// Slice operator.
    inout(E)[] opSlice(size_t i, size_t j) inout @trusted return scope
    {
        return slice()[i .. j];
    }
    ///
    inout(E)[] opSlice() inout return scope
    {
        return this.opSlice(0, _length);
    }

    /// Index assign operator.
    ref E opIndexAssign(V)(V value, size_t i) @trusted return scope
    {
        static if (hasElaborateDestructor!E)
            move(*(cast(MutableE*)(&value)), _mptr[i]); // TODO is this correct?
        else static if (hasIndirections!E && // TODO `hasAliasing` instead?
                        !isMutable!E)
            static assert("Cannot modify constant elements with indirections");
        else
            slice()[i] = value;
        return slice()[i];
    }

    /// Slice assign operator.
    E[] opSliceAssign(V)(V value, size_t i, size_t j) @trusted return scope
    {
        return slice()[i .. j] = value;
    }
    /// ditto
    E[] opSliceAssign(V)(V value) @trusted return scope
    {
        return slice()[] = value;
    }

    /// Get front element reference.
    ref inout(E) front() inout @trusted return scope
    {
        assert(!empty);
        return slice()[0];
    }

    /// Get back element reference.
    ref inout(E) back() inout @trusted return scope
    {
        assert(!empty);
        return slice()[_length - 1];
    }

    /** Inserts the given value into the end of the array.
     */
    void pushBack(E[] values...) @trusted
    {
        reserve(_length + values.length);
        foreach (const ix, const ref value; values)
        {
            static if (hasIndirections!E && // TODO `hasAliasing` instead?
                       !isMutable!E)
            {
                static assert("Cannot modify constant elements with indirections");
            }
            else
            {
                mslice()[_length + ix] = value;
            }
        }
        _length += values.length;
    }
    /// ditto
    alias put = pushBack;

    /** ~= operator overload */
    void opOpAssign(string op)(E value) if (op == "~")
    {
        pushBack(value);
    }

    /// Helper slice.
    private inout(E)[] slice() inout return scope
    {
        return _ptr[0 .. _length];
    }

    /// Helper mutable slice.
    private MutableE[] mslice() inout return scope
    {
        return _mptr[0 .. _length];
    }

    /// Reserve room for `newCapacity`.
    private void reserve(size_t newCapacity) @trusted
    {
        if (newCapacity <= capacity) { return; }

        static if (shouldAddGCRange!E)
        {
            gc_removeRange(_mptr);
        }

        import std.math : nextPow2;
        reallocateAndSetCapacity(newCapacity.nextPow2);

        static if (shouldAddGCRange!E)
        {
            gc_addRange(_mptr, _capacity * E.sizeof);
        }
    }

    /// Reallocate storage.
    private void reallocateAndSetCapacity(size_t newCapacity) pure nothrow @trusted
    {
        _capacity = newCapacity;
        static if (useGCAllocation)
        {
            _ptr = cast(E*)GC.realloc(_mptr, E.sizeof * _capacity);
        }
        else                    // @nogc
        {
            _ptr = cast(E*)realloc(_mptr, E.sizeof * _capacity);
            assert(_mptr, "Reallocation failed");
        }
    }

    /// Mutable pointer.
    private MutableE* _mptr() const @trusted
    {
        return cast(typeof(return))_ptr;
    }

private:
    static if (useGCAllocation)
        E* _ptr; // GC-allocated store pointer. See also: http://forum.dlang.org/post/iubialncuhahhxsfvbbg@forum.dlang.org
    else
        @nogc E* _ptr;   // non-GC-allocated store pointer
    size_t _capacity;            // store capacity
    size_t _length;              // store length
}

template shouldAddGCRange(T)
{
    import std.traits : hasIndirections;
    enum shouldAddGCRange = hasIndirections!T; // TODO unless all pointers members are tagged as @nogc (as in `BasicArray` and `BasicStore`)
}


@safe pure nothrow @nogc unittest
{
    const length = 3;

    alias E = const(int);
    alias A = BasicArray!(E);

    A a;
    assert(a.empty);
    assert(a.length == 0);
    assert(a.capacity == 0);
    assert(a[] == []);

    auto b = BasicArray!int.withLength(3);
    assert(!b.empty);
    assert(b.length == 3);
    assert(b.capacity == 3);
    b[0] = 1;
    b[1] = 2;
    b[2] = 3;
    assert(b[] == [1, 2, 3].s);

    b[] = [4, 5, 6].s;
    assert(b[] == [4, 5, 6].s);

    const c = BasicArray!int.withCapacity(3);
    assert(c.empty);
    assert(c.capacity == 3);
    assert(c[] == []);

    // TODO this should fail with -dip1000
    auto f() @safe
    {
        A a;
        return a[];
    }
    auto d = f();
}

@safe pure nothrow @nogc unittest
{
    const length = 3;

    alias E = const(int*);
    alias A = BasicArray!(E);

    A a;

    a.length = 1;
    assert(a.length == 1);
    assert(a.capacity >= 1);

    a[0] = E.init;

    a.pushBack(E.init);
    a ~= E.init;

    assert(a.length == 3);
}

version(unittest)
{
    import array_help : s;
    // import dbgio : dln;
}
