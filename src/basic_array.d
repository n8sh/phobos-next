module basic_array;

import std.traits : Unqual;

/** Array type with deterministic control of memory. The memory allocated for
   the array is reclaimed as soon as possible; there is no reliance on the
   garbage collector. Array uses malloc, realloc and free for managing its own
   memory.

   Use `std.bitmanip.BitArray` for array container storing boolean values.

   TODO make use of `Allocator` parameter when non-`null`

   TODO Use correct growth factor:
   - https://github.com/facebook/folly/blob/master/folly/docs/FBVector.md

   See also: https://github.com/facebook/folly/blob/master/folly/docs/FBVector.md
 */
struct BasicArray(T,
                  alias Allocator = null) // null means means to qcmeman functions
    if (!is(Unqual!T == bool))
{
    import std.range : isInputRange, ElementType, isInfinite;
    import std.traits : Unqual, hasElaborateDestructor, hasIndirections, hasAliasing, isMutable, TemplateOf, isArray, isAssignable, isCopyable;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;
    import std.algorithm : move, moveEmplace;

    /// Mutable element type.
    private alias MutableE = Unqual!T;

    /// Template for type of `this`.
    private alias ThisTemplate = TemplateOf!(This);

    /// Same type as this but with mutable element type.
    private alias MutableThis = ThisTemplate!(MutableE, Allocator);

    /// Type of `this`.
    private alias This = typeof(this);

    /// Is `true` if `U` can be assign to the element type `T` of `this`.
    enum isElementAssignable(U) = isAssignable!(MutableE, U);
    enum isElementMovable(U) = is(typeof(MutableE == Unqual!U));
    enum isElementAssignableOrMovable(U) = isAssignable!(U) || isElementMovable!(U);

    /// True if elements need move.
    enum needsMove(T) = !isCopyable!T || hasIndirections!T;

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
        if (isElementAssignable!U &&
            isCopyable!U)       // prevent accidental move of l-value `values`
    {
        if (values.length == 1) // TODO branch should be detected at compile-time
        {
            // twice as fast as array assignment below
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

    enum isConstructableFromRange(R) = (isInputRange!R &&
                                        !isInfinite!R &&
                                        isElementAssignableOrMovable!(ElementType!R));

    /// Construct from range of element `values`.
    this(R)(R values) @trusted
        if (!isArray!R &&
            isConstructableFromRange!R)
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
            foreach (ref value; move(values))
            {
                static if (isCopyable!(ElementType!R))
                {
                    insertBack(value);
                }
                else
                {
                    insertBackMove(value);
                }
            }
        }
    }

    /// Construct from uncopyable range of uncopyable `values`.
    this(R)(R values) @trusted
        if (!isCopyable!R &&
            !isCopyable!(ElementType!R) &&
            isElementAssignable!(ElementType!R))
    {
        static assert(false, "TODO implement");
    }

    /** Construct using
        - initial capacity `initialCapacity`,
        - initial length `initialLength`
        - and zeroing-flag `zero`.
    */
    private this(size_t initialCapacity,
                 size_t initialLength = 0,
                 bool zero = true) @trusted
    {
        assert(initialCapacity >= initialLength);
        _capacity = initialCapacity;
        _ptr = This.allocate(initialCapacity, zero);
        _length = initialLength;
    }

    static if (isCopyable!T)
    {
        /// Copy construction.
        this(this) @trusted
        {
            MutableE* newPtr = This.allocate(_length, false);
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
        static if (hasElaborateDestructor!T)
        {
            destroyElements();
        }
        static if (shouldAddGCRange!T)
        {
            gc_removeRange(_ptr);
        }
        free(_mptr);
    }

    /// Destroy elements.
    static if (hasElaborateDestructor!T)
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

    /** Allocate heap regionwith `initialCapacity` number of elements of type `T`.
        If `zero` is `true` they will be zero-initialized.
    */
    private static MutableE* allocate(size_t initialCapacity, bool zero = false)
    {
        typeof(return) ptr = null;

        if (zero) { ptr = cast(typeof(return))calloc(initialCapacity, T.sizeof); }
        else      { ptr = cast(typeof(return))malloc(initialCapacity * T.sizeof); }
        assert(ptr, "Allocation failed");

        static if (shouldAddGCRange!T)
        {
            gc_addRange(ptr, initialCapacity * T.sizeof);
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

    /// Calculate D associative array (AA) key hash.
    size_t toHash() const @trusted pure nothrow
    {
        import core.internal.hash : hashOf;
        typeof(return) hash = this.length;
        foreach (immutable i; 0 .. this.length)
        {
            hash ^= _ptr[i].hashOf;
        }
        return hash;
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

    /** Ensures sufficient capacity to accommodate for requestedCapacity number
        of elements. If `requestedCapacity` < `capacity`, this method does
        nothing.
     */
    void reserve(size_t requestedCapacity) @trusted
    {
        if (requestedCapacity <= capacity) { return; }

        static if (shouldAddGCRange!T)
        {
            gc_removeRange(_mptr);
        }

        // growth factor
        // Motivation: https://github.com/facebook/folly/blob/master/folly/docs/FBVector.md#memory-handling
        reallocateAndSetCapacity(3*requestedCapacity/2); // use 1.5 as Facebook's `fbvector` does
        // import std.math : nextPow2;
        // reallocateAndSetCapacity(requestedCapacity.nextPow2);

        static if (shouldAddGCRange!T)
        {
            gc_addRange(_mptr, _capacity * T.sizeof);
        }
    }

    /// Index support.
    ref inout(T) opIndex(size_t i) inout return scope
    {
        return slice()[i];
    }

    /// Slice support.
    inout(T)[] opSlice(size_t i, size_t j) inout return scope
    {
        return slice()[i .. j];
    }
    ///
    inout(T)[] opSlice() inout return scope
    {
        return opSlice(0, _length);
    }

    /// Index assignment support.
    ref T opIndexAssign(U)(U value, size_t i) @trusted return scope
    {
        static if (hasElaborateDestructor!T)
        {
            move(*(cast(MutableE*)(&value)), _mptr[i]); // TODO is this correct?
        }
        else static if (hasIndirections!T && // TODO `hasAliasing` instead?
                        !isMutable!T)
        {
            static assert("Cannot modify constant elements with indirections");
        }
        else
        {
            slice()[i] = value;
        }
        return slice()[i];
    }

    /// Slice assignment support.
    T[] opSliceAssign(U)(U value) return scope
    {
        return slice()[] = value;
    }

    /// ditto
    T[] opSliceAssign(U)(U value, size_t i, size_t j) return scope
    {
        return slice()[i .. j] = value;
    }

    /// Get front element reference.
    ref inout(T) front() inout return scope @property
    {
        // TODO use?: enforce(!empty); emsi-containers doesn't, std.container.Array does
        return slice()[0];
    }

    /// Get back element reference.
    ref inout(T) back() inout return scope @property
    {
        // TODO use?: enforce(!empty); emsi-containers doesn't, std.container.Array does
        return slice()[_length - 1];

    }

    /** Move `value` into the end of the array.
     */
    pragma(inline, true)
    void insertBackMove()(ref T value) @trusted
    {
        reserve(_length + 1);
        moveEmplace(value, _mptr[_length]);
        _length += 1;
    }

    /** Insert `value` into the end of the array.

        TODO rename to `insertBack` and make this steal scalar calls over
        insertBack(U)(U[] values...) overload below
     */
    pragma(inline, true)
    void insertBack1(T value) @trusted
    {
        reserve(_length + 1);
        static if (isCopyable!T)
        {
            _mptr[_length] = value;
        }
        else
        {
            moveEmplace(*cast(MutableE*)(&value),
                        _mptr[_length]); // TODO remove `move` when compiler does it for us
        }
        _length += 1;
    }

    /** Insert unmoveable `value` into the end of the array.
     */
    pragma(inline, true)
    void insertBack()(T value) @trusted
        if (!isCopyable!T)
    {
        reserve(_length + 1);
        moveEmplace(*cast(MutableE*)(&value),
                    _mptr[_length]); // TODO remove `move` when compiler does it for us
        _length += 1;
    }

    /** Insert `values` into the end of the array.
     */
    void insertBack(U)(U[] values...) @trusted
        if (isElementAssignable!U &&
            isCopyable!U)       // prevent accidental move of l-value `values`
    {
        if (values.length == 1) // TODO branch should be detected at compile-time
        {
            // twice as fast as array assignment below
            return insertBack1(values[0]);
        }
        static if (is(T == immutable(T)))
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
    void insertBack(R)(R values)
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
    void opOpAssign(string op)(T[] values...)
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
    private inout(T)[] slice() inout return scope @trusted
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
        _ptr = cast(T*)realloc(_mptr, T.sizeof * _capacity);
        assert(_mptr, "Reallocation failed");
    }

    /// Mutable pointer.
    private MutableE* _mptr() const return scope @trusted
    {
        return cast(typeof(return))_ptr;
    }

private:
    // defined here https://dlang.org/phobos/std_experimental_allocator_gc_allocator.html#.GCAllocator
    static if (is(Allocator == std.experimental.allocator.gc_allocator.GCAllocator))
        T* _ptr;                // GC-allocated store pointer
    else
        @nogc T* _ptr;          // non-GC-allocated store pointer
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
    alias T = int;
    alias A = BasicArray!(T);

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
    alias T = int;
    alias A = BasicArray!(T);

    A a;

    a.length = 1;
    assert(a.length == 1);
    assert(a.capacity >= 1);

    a[0] = 10;

    a.insertBack(11, 12);

    a ~= T.init;
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
    alias T = const(int);
    alias A = BasicArray!(T);

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
    alias T = const(int);
    alias A = BasicArray!(T);

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
    private static struct SomeUncopyableStruct
    {
        @disable this(this);
        int x;
    }
}

/// non-copyable element type
@safe pure nothrow /*@nogc*/ unittest
{
    alias A = BasicArray!(SomeUncopyableStruct);
    A a0 = A();

    const s = SomeUncopyableStruct(1);
    const a = [SomeUncopyableStruct(1)];

    a0.insertBack(SomeUncopyableStruct(1));

    assert(a0[] == a[]);

    // a0 ~= A(1);
    A a42 = A(42);
}

/** Non-copyable variant of `BasicArray`.
 */
struct UniqueBasicArray(T,
                        alias Allocator = null) // null means means to qcmeman functions
    if (!is(Unqual!T == bool))
{
    import std.range : isInputRange, ElementType, isInfinite, isCopyable;

    @disable this(this);        // no copy construction

    /// Construct from element `values`.
    this(U)(U[] values...)
        if (Super.isElementAssignable!U &&
            isCopyable!U)       // prevent accidental move of l-value `values`
    {
        basicArray = Super(values);
    }

    /// Construct from range of element `values`.
    this(R)(R values)
        if (Super.isConstructableFromRange!R)
    {
        basicArray = Super(values);
    }

    /// Construct from uncopyable range of uncopyable `values`.
    this(R)(R values) @trusted
        if (!isCopyable!R &&
            !isCopyable!(ElementType!R) &&
            Super.isElementAssignable!(ElementType!R))
    {
        static assert(false, "TODO implement");
    }

    /// Returns: shallow duplicate of `this`.
    static if (isCopyable!T)
    {
        @property Super.MutableThis dup() const // `MutableThis` mimics behaviour of `dup` for builtin D arrays
        {
            return typeof(return)(slice());
        }
    }

    alias Super = BasicArray!(T, Allocator);
    Super basicArray;
    alias basicArray this;
}

/// construct from uncopyable scalar
@safe pure nothrow @nogc unittest
{
    alias T = const(int);
    alias A = UniqueBasicArray!(T);
    const a = A(17);
    assert(a[] == [17].s);
}

/// construct from slice of copyable type
@safe pure nothrow unittest
{
    alias T = const(int);
    alias A = UniqueBasicArray!(T);
    const a = A([17]);
    assert(a[] == [17].s);
}

/// construct from scalar of uncopyable type
@safe pure nothrow @nogc unittest
{
    alias A = UniqueBasicArray!(SomeUncopyableStruct);
    // const a = A(SomeUncopyableStruct(17));
}

/// construct from slice of uncopyable type
@safe pure nothrow unittest
{
    alias A = UniqueBasicArray!(SomeUncopyableStruct);
    // const a = A([SomeUncopyableStruct(17)]);
}

/// check disabled copying
@safe pure nothrow @nogc unittest
{
    alias T = const(int);
    alias A = UniqueBasicArray!(T);

    auto a = A.withLength(3);

    static assert(!__traits(compiles, { A b = a; })); // copying disabled
}

version(unittest)
{
    import array_help : s;
}
