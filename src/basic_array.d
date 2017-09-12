module basic_array;

private struct BasicArray(E, alias Allocator = null) // null means means to qcmeman functions
{
    import std.range : isIterable, hasLength;
    import std.traits : Unqual, hasElaborateDestructor, hasIndirections, hasAliasing, isMutable, isCopyable, TemplateOf, isArray;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;

    /// Mutable element type.
    private alias MutableE = Unqual!E;

    /// Template for type of `this`.
    private alias ThisTemplate = TemplateOf!(This);

    /// Same type as this but with mutable element type.
    private alias MutableThis = ThisTemplate!(MutableE, Allocator);

    /// Type of `this`.
    private alias This = typeof(this);

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

    /// Construct from range `values`.
    static This fromValues(R)(R values) @trusted
        if (isIterable!R)
    {
        return This(values);
    }

    /// Construct from range `values`.
    this(R)(R values) @trusted
        if (isIterable!R)
    {
        static if (hasLength!R)
        {
            reserve(values.length);
            _length = values.length;

            static if (isArray!R)
            {
                _mptr[0 .. _length] = values; // TODO prevent overlap check?
            }
            else
            {
                import std.algorithm : copy;
                copy(values, _mptr[0 .. _length]);
            }
        }
        else
        {
            foreach (const i, ref value; values)
            {
                pushBack(value);
            }
        }
    }

    private this(size_t initialCapacity,
                 size_t initialLength = 0,
                 bool zero = true) @trusted
    {
        assert(initialCapacity >= initialLength);
        _capacity = initialCapacity;
        _ptr = allocate(initialCapacity, zero);
        _length = initialLength;
    }

    static if (isCopyable!E)
    {
        this(this) @trusted
        {
            MutableE* newPtr = allocate(_length, false);
            _capacity = _length;
            newPtr[0 .. _length] = slice();
            _ptr = newPtr;
        }
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

    /// Release internal store.
    pragma(inline, true)
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
        free(_mptr);
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

    /// Reset internal data.
    pragma(inline, true)
    private void resetInternalData()
    {
        _ptr = null;
        _length = 0;
        _capacity = 0;
    }

    /** Allocate heap regionwith `initialCapacity` number of elements of type `E`.
        If `zero` is `true` they will be zero-initialized.
    */
    private static MutableE* allocate(size_t initialCapacity, bool zero = false)
    {
        typeof(return) ptr = null;

        if (zero) { ptr = cast(typeof(return))calloc(initialCapacity, E.sizeof); }
        else      { ptr = cast(typeof(return))malloc(initialCapacity * E.sizeof); }
        assert(ptr, "Allocation failed");

        static if (shouldAddGCRange!E)
        {
            gc_addRange(ptr, initialCapacity * E.sizeof);
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

    /** Ensures sufficient capacity to accommodate for newCapacity number of
        elements. If `newCapacity` < `capacity`, this method does nothing.
     */
    void reserve(size_t newCapacity) @trusted
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
        return opSlice(0, _length);
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
    private inout(E)[] slice() inout return scope @trusted
    {
        return _ptr[0 .. _length];
    }

    /// Helper mutable slice.
    private MutableE[] mslice() return scope @trusted
    {
        return _mptr[0 .. _length];
    }

    /// Reallocate storage.
    private void reallocateAndSetCapacity(size_t newCapacity) pure nothrow @trusted
    {
        _capacity = newCapacity;
        _ptr = cast(E*)realloc(_mptr, E.sizeof * _capacity);
        assert(_mptr, "Reallocation failed");
    }

    /// Mutable pointer.
    private MutableE* _mptr() const @trusted
    {
        return cast(typeof(return))_ptr;
    }

private:
    @nogc E* _ptr;              // non-GC-allocated store pointer
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

    const e = BasicArray!int([1, 2, 3, 4].s);
    assert(e.length == 4);
    assert(e[] == [1, 2, 3, 4].s);
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

@safe pure nothrow @nogc unittest
{
    const length = 3;

    alias E = const(int);
    alias A = BasicArray!(E);

    auto a = A([1, 2, 3].s);
    A b = a;                    // copy construction enabled

    assert(a[] == b[]);         // same content
    assert(a[].ptr !is b[].ptr);

    assert(b[] == [1, 2, 3].s);
    assert(b.length == 3);

    auto c = A.fromValues([1, 2, 3].s);
}

private struct UniqueBasicArray(E, alias Allocator = null) // null means means to qcmeman functions
{
    @disable this(this);        // no copy construction

    /// Returns: shallow duplicate of `this`.
    @property basicArray.MutableThis dup() const @trusted // `MutableThis` mimics behaviour of `dup` for builtin D arrays
    {
        return typeof(return)(slice());
    }

    BasicArray!(E, Allocator) basicArray;
    alias basicArray this;
}

@safe pure nothrow @nogc unittest
{
    const length = 3;

    alias E = const(int);
    alias A = UniqueBasicArray!(E);

    auto a = A.withLength(length);

    static assert(!__traits(compiles, { A b = a; })); // copying disabled

    // TODO auto b = a.dup;
}

version(unittest)
{
    import array_help : s;
    // import dbgio : dln;
}
