module basic_array;

import std.traits : Unqual;

/** Array type with deterministic control of memory. The memory allocated for
   the array is reclaimed as soon as possible; there is no reliance on the
   garbage collector. Array uses malloc, realloc and free for managing its own
   memory.

   Use `std.bitmanip.BitArray` for array container storing boolean values.

   TODO make use of `Allocator` parameter when non-`null`
 */
struct BasicArray(E,
                  alias Allocator = null) // null means means to qcmeman functions
    if (!is(Unqual!T == bool))
{
    import std.range : isInputRange, ElementType, isInfinite;
    import std.traits : Unqual, hasElaborateDestructor, hasIndirections, hasAliasing, isMutable, isCopyable, TemplateOf, isArray, isAssignable;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;

    /// Mutable element type.
    private alias MutableE = Unqual!E;

    /// Template for type of `this`.
    private alias ThisTemplate = TemplateOf!(This);

    /// Same type as this but with mutable element type.
    private alias MutableThis = ThisTemplate!(MutableE, Allocator);

    /// Type of `this`.
    private alias This = typeof(this);

    /// Is `true` if `U` can be assign to the element type `E` of `this`.
    enum isElementAssignable(U) = isAssignable!(MutableE, U);

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

    /// Construct from element `values`.
    this(U)(U[] values...) @trusted
        if (isElementAssignable!U)
    {
        if (values.length == 1) // TODO branch should be detected at compile-time
        {
            // twice as fast as default array assignment below
            _capacity = 1;
            _length = 1;
            _ptr = This.allocate(1);
            _mptr[0] = values[0];
            return;
        }
        reserve(values.length);
        _length = values.length;
        _mptr[0 .. _length] = values; // array assignment
    }

    /// Construct from range of element `values`.
    this(R)(R values) @trusted
        if (isInputRange!R &&
            !isInfinite!R &&
            !isArray!R &&
            isElementAssignable!(ElementType!R))
    {
        import std.range : hasLength;
        static if (hasLength!R)
        {
            reserve(values.length);
            _length = values.length;
            import std.algorithm : copy;
            copy(values, _mptr[0 .. _length]);
        }
        else
        {
            foreach (ref value; values)
            {
                insertBack(value);
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

    /** Comparison for equality. */
    pragma(inline, true)
    bool opEquals(in This rhs) const
    {
        return opEquals(rhs);
    }

    /// ditto
    bool opEquals(ref in This rhs) const
    {
        if (empty) return rhs.empty;
        if (rhs.empty) return false;
        return slice() == rhs.slice();
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
    ref E opIndexAssign(U)(U value, size_t i) @trusted return scope
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
    E[] opSliceAssign(U)(U value, size_t i, size_t j) @trusted return scope
    {
        return slice()[i .. j] = value;
    }

    /// ditto
    E[] opSliceAssign(U)(U value) @trusted return scope
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

    /** Insert `value` into the end of the array.

        TODO rename to `insertBack` and make this steal scalar calls over
        insertBack(U)(U[] values...) overload below
     */
    pragma(inline, true)
    void insertBack1(E value) @trusted
    {
        reserve(_length + 1);
        _mptr[_length] = value;
        _length += 1;
    }

    /** Insert `values` into the end of the array.
     */
    void insertBack(U)(U[] values...) @trusted
        if (isElementAssignable!U)
    {
        if (values.length == 1) // TODO branch should be detected at compile-time
        {
            // twice as fast as default array assignment below
            return insertBack1(values[0]);
        }
        static if (is(E == immutable(E)))
        {
            /* An array of immutable values cannot overlap with the `this`
               mutable array container data, which entails no need to check for
               overlap.
            */
            reserve(_length + values.length);
            _mptr[_length .. _length + values.length] = values;
        }
        else
        {
            import overlapping : overlaps;
            if (_ptr == values.ptr) // called for instances as: `this ~= this`
            {
                reserve(2*_length); // invalidates `values.ptr`
                foreach (immutable i; 0 .. _length)
                {
                    _mptr[_length + i] = _ptr[i];
                }
            }
            else if (overlaps(this[], values[]))
            {
                assert(false, `TODO Handle overlapping arrays`);
            }
            else
            {
                reserve(_length + values.length);
                _mptr[_length .. _length + values.length] = values;
            }
        }
        _length += values.length;
    }

    /** Insert `values` into the end of the array.
     */
    void insertBack(R)(R values) @trusted
        if (isInputRange!R &&
            !isInfinite!R &&
            !isArray!R &&
            isElementAssignable!(ElementType!R))
    {
        import std.range : hasLength;
        static if (hasLength!R)
        {
            reserve(_length + values.length);
            import std.algorithm : copy;
            copy(values, _mptr[_length .. _length + values.length]);
            _length += values.length;
        }
        else
        {
            foreach (ref value; values)
            {
                insertBack(value);
            }
        }
    }

    /// ditto
    alias put = insertBack;

    /// ditto
    void opOpAssign(string op)(E[] values...)
        if (op == "~")
    {
        insertBack(values);
    }
    /// ditto
    void opOpAssign(string op, R)(R values)
        if (op == "~" &&
            isInputRange!R &&
            !isInfinite!R &&
            isElementAssignable!(ElementType!R))
    {
        insertBack(values);
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
    // defined here https://dlang.org/phobos/std_experimental_allocator_gc_allocator.html#.GCAllocator
    static if (is(Allocator == std.experimental.allocator.gc_allocator.GCAllocator))
        E* _ptr;                // GC-allocated store pointer
    else
        @nogc E* _ptr;          // non-GC-allocated store pointer
    size_t _capacity;           // store capacity
    size_t _length;             // store length
}

private template shouldAddGCRange(T)
{
    import std.traits : hasIndirections;
    enum shouldAddGCRange = hasIndirections!T; // TODO unless all pointers members are tagged as @nogc (as in `BasicArray` and `BasicStore`)
}

/// construct and append from slices
@safe pure nothrow @nogc unittest
{
    alias E = int;
    alias A = BasicArray!(E);

    auto a = A([10, 11, 12].s);

    a ~= a[];
    assert(a[] == [10, 11, 12,
                   10, 11, 12].s);

    a ~= false;
    assert(a[] == [10, 11, 12,
                   10, 11, 12, 0].s);
}

@safe pure nothrow @nogc unittest
{
    alias E = int;
    alias A = BasicArray!(E);

    A a;

    a.length = 1;
    assert(a.length == 1);
    assert(a.capacity >= 1);

    a[0] = 10;

    a.insertBack(11, 12);

    a ~= E.init;
    a.insertBack([3].s);
    assert(a[] == [10, 11, 12, 0, 3].s);

    import std.algorithm : filter;

    a.insertBack([42].s[].filter!(_ => _ is 42));
    assert(a[] == [10, 11, 12, 0, 3, 42].s);

    a.insertBack([42].s[].filter!(_ => _ !is 42));
    assert(a[] == [10, 11, 12, 0, 3, 42].s);

    a ~= a[];
    assert(a[] == [10, 11, 12, 0, 3, 42,
                   10, 11, 12, 0, 3, 42].s);
}

@safe pure nothrow @nogc unittest
{
    alias E = const(int);
    alias A = BasicArray!(E);

    A a;                        // default construction allowed
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
    alias E = const(int);
    alias A = BasicArray!(E);

    auto a = A([1, 2, 3].s);
    A b = a;                    // copy construction enabled

    assert(a[] == b[]);          // same content
    assert(a[].ptr !is b[].ptr); // but not the same

    assert(b[] == [1, 2, 3].s);
    assert(b.length == 3);

    b ~= 4;
    assert(a != b);
    a.clear();
    assert(a != b);
    b.clear();
    assert(a == b);

    auto c = A([1, 2, 3].s);

    auto d = A(1, 2, 3);
}

version(unittest)
{
    /// non-copyable element type
    private static struct S
    {
        @disable this(this);
        int x;
    }
}

/// non-copyable element type
@safe pure nothrow @nogc unittest
{
    // alias A = BasicArray!(S);
}

struct UniqueBasicArray(E,
                        alias Allocator = null) // null means means to qcmeman functions
    if (!is(Unqual!T == bool))
{
    import std.range : isInputRange, ElementType, isInfinite;

    @disable this(this);        // no copy construction

    /// Construct from element `values`.
    this(E[] values...) @trusted
    {
        basicArray = typeof(basicArray)(values);
    }

    /// Construct from range of element `values`.
    this(R)(R values) @trusted
        if (isInputRange!R &&
            !isInfinite!R &&
            basicArray.isElementAssignable!(ElementType!R))
    {
        basicArray = typeof(basicArray)(values);
    }

    /// Returns: shallow duplicate of `this`.
    @property basicArray.MutableThis dup() const @trusted // `MutableThis` mimics behaviour of `dup` for builtin D arrays
    {
        return typeof(return)(slice());
    }

    BasicArray!(E, Allocator) basicArray;
    alias basicArray this;
}

/// construct from scalar
@safe pure nothrow @nogc unittest
{
    alias E = const(int);
    alias A = UniqueBasicArray!(E);
    auto a = A(17);
    assert(a[] == [17].s);
}

/// check disabled copying
@safe pure nothrow @nogc unittest
{
    alias E = const(int);
    alias A = UniqueBasicArray!(E);

    auto a = A.withLength(3);

    static assert(!__traits(compiles, { A b = a; })); // copying disabled
}

version(unittest)
{
    import array_help : s;
}
