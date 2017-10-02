module basic_copyable_array;

import std.traits : Unqual;

/** Array type with deterministic control of memory. The memory allocated for
    the array is reclaimed as soon as possible; there is no reliance on the
    garbage collector. Array uses malloc, realloc and free for managing its own
    memory.

    Use `std.bitmanip.BitArray` for array container storing boolean values.

    TODO Add OutputRange.writer support as
    https://github.com/burner/StringBuffer/blob/master/source/stringbuffer.d#L45

    TODO Call gc_addRange in all insertBack* functions

    TODO make use of `Allocator` parameter when non-`null`

    TODO Use `std.traits.areCopyCompatibleArrays`

    See also: https://github.com/facebook/folly/blob/master/folly/docs/FBVector.md
*/
struct CopyableArray(T,
                     alias Allocator = null) // null means means to qcmeman functions
    if (!is(Unqual!T == bool))
{
    import std.range : isInputRange, isIterable, ElementType, isInfinite;
    import std.traits : Unqual, hasElaborateDestructor, hasIndirections, hasAliasing, isMutable, TemplateOf, isArray, isAssignable, isCopyable;
    import std.algorithm : move, moveEmplace;

    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;
    import container_traits : shouldAddGCRange;

    /// Mutable element type.
    private alias MutableE = Unqual!T;

    /// Template for type of `this`.
    private alias ThisTemplate = TemplateOf!(typeof(this));

    /// Same type as this but with mutable element type.
    private alias MutableThis = ThisTemplate!(MutableE, Allocator);

    /// Is `true` if `U` can be assign to the element type `T` of `this`.
    enum isElementAssignable(U) = isAssignable!(MutableE, U);
    enum isElementMovable(U) = is(typeof(MutableE == Unqual!U));
    enum isElementAssignableOrMovable(U) = isAssignable!(U) || isElementMovable!(U);

    /// True if elements need move.
    enum needsMove(T) = !isCopyable!T || hasElaborateDestructor!T;

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    pragma(inline, true)
    static typeof(this) withLength(size_t initialLength)
    {
        return withCapacityLengthZero(initialLength, initialLength, true);
    }

    /// Returns: an array with initial capacity `initialCapacity`.
    pragma(inline, true)
    static typeof(this) withCapacity(size_t initialCapacity)
    {
        return withCapacityLengthZero(initialCapacity, 0, false);
    }

    /** Construct using
        - initial capacity `initialCapacity`,
        - initial length `initialLength`
        - and zeroing-flag `zero`.
    */
    private static typeof(this) withCapacityLengthZero(size_t initialCapacity,
                                                       size_t initialLength,
                                                       bool zero) @trusted
    {
        assert(initialCapacity >= initialLength);
        // TODO use Store constructor:
        typeof(this) that;
        that._ptr = typeof(this).allocate(initialCapacity, zero);
        that._capacity = initialCapacity;
        that._length = initialLength;
        return that;
    }

    /// Construct from uncopyable element `value`.
    this()(T value) @trusted
        if (!isCopyable!T)
    {
        _ptr = typeof(this).allocate(1, false);
        _capacity = 1;
        _length = 1;
        moveEmplace(value, _mptr[0]); // TODO remove `moveEmplace` when compiler does it for us
    }

    /// Construct from uncopyable element `value`.
    this(U)(U value) @trusted
        if (isCopyable!U &&
            isElementAssignable!U)
    {
        _ptr = typeof(this).allocate(1, false);
        _capacity = 1;
        _length = 1;
        _mptr[0] = value;
    }

    /// Construct from element `values`.
    this(U)(U[] values...) @trusted
        if (isCopyable!U &&
            isElementAssignable!U) // prevent accidental move of l-value `values` in array calls
    {
        if (values.length == 1) // TODO branch should be detected at compile-time
        {
            // twice as fast as array assignment below
            _ptr = typeof(this).allocate(1, false);
            _capacity = 1;
            _length = 1;
            _mptr[0] = values[0];
            return;
        }
        reserve(values.length);
        _length = values.length;
        _mptr[0 .. _length] = values; // array assignment
    }

    /** Is `true` iff constructable from the iterable (or range) `I`.
     */
    enum isAssignableFromElementsOfRefIterableStruct(I) = (is(I == struct) && // exclude class ranges for aliasing control
                                                           isRefIterable!I && // elements may be non-copyable
                                                           !isInfinite!I &&
                                                           isElementAssignable!(ElementType!I));

    /// Construct from the elements `values`.
    this(R)(R values) @trusted
        if (isAssignableFromElementsOfRefIterableStruct!R)
    {
        import std.range : hasLength;
        static if (hasLength!R)
        {
            reserve(values.length);
        }
        static if (hasLength!R &&
                   isCopyable!(ElementType!R))
        {
            _length = values.length;
            import std.algorithm : copy;
            copy(values, _mptr[0 .. _length]); // TODO better to use foreach instead?
        }
        else
        {
            foreach (ref value; move(values)) // TODO remove `move` when compiler does it for us
            {
                static if (isCopyable!(ElementType!R))
                {
                    insertBack(value);
                }
                else
                {
                    insertBackMove(value); // steal element
                }
            }
        }
    }

    // optional copy construction
    static if (isCopyable!T)
    {
        /// Copy construction.
        this(this) @trusted
        {
            MutableE* newPtr = typeof(this).allocate(_length, false);
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
        _capacity = 0;
        _length = 0;
    }

    /** Allocate heap regionwith `initialCapacity` number of elements of type `T`.
        If `zero` is `true` they will be zero-initialized.
    */
    private static MutableE* allocate(size_t initialCapacity, bool zero)
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
    bool opEquals(in typeof(this) rhs) const
    {
        return slice() == rhs.slice();
    }
    /// ditto
    pragma(inline, true)
    bool opEquals(in ref typeof(this) rhs) const
    {
        return slice() == rhs.slice();
    }
    /// ditto
    pragma(inline, true)
    bool opEquals(U)(in U[] rhs) const
        if (is(typeof(T[].init == U[].init)))
    {
        return slice() == rhs;
    }

    /// Calculate D associative array (AA) key hash.
    size_t toHash() const @trusted
    {
        import core.internal.hash : hashOf;
        typeof(return) hash = this.length;
        foreach (immutable i; 0 .. this.length)
        {
            hash ^= _ptr[i].hashOf;
        }
        return hash;
    }

    static if (isCopyable!T)
    {
        /** Construct a string representation of `this` at `sink`.
         */
        void toString(scope void delegate(const(char)[]) sink) const
        {
            sink("[");
            foreach (const ix, ref value; slice())
            {
                import std.format : formattedWrite;
                sink.formattedWrite("%s", value);
                if (ix + 1 < length) { sink(", "); } // separator
            }
            sink("]");
        }
    }

    /// Check if empty.
    pragma(inline, true)
    bool empty() const { return _length == 0; }

    /// Get length.
    pragma(inline, true)
    @property size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    /// Set length to `newLength`.
    pragma(inline, true)
    @property void length(size_t newLength)
    {
        reserve(newLength);
        _length = newLength;
    }

    /// Get capacity.
    pragma(inline, true)
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
        reallocateAndSetCapacity(3*requestedCapacity/2); // use 1.5 like Facebook's `fbvector` does
        // import std.math : nextPow2;
        // reallocateAndSetCapacity(requestedCapacity.nextPow2);

        static if (shouldAddGCRange!T)
        {
            gc_addRange(_mptr, _capacity * T.sizeof);
        }
    }

    /// Index support.
    pragma(inline, true)
    scope ref inout(T) opIndex(size_t i) inout return
    {
        return slice()[i];
    }

    /// Slice support.
    pragma(inline, true)
    scope inout(T)[] opSlice(size_t i, size_t j) inout return
    {
        return slice()[i .. j];
    }
    /// ditto
    pragma(inline, true)
    scope inout(T)[] opSlice() inout return
    {
        return slice();
    }

    /// Index assignment support.
    scope ref T opIndexAssign(U)(U value, size_t i) @trusted return
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
    pragma(inline, true)
    scope T[] opSliceAssign(U)(U value) return
    {
        return slice()[] = value;
    }

    /// ditto
    pragma(inline, true)
    scope T[] opSliceAssign(U)(U value, size_t i, size_t j) return
    {
        return slice()[i .. j] = value;
    }

    /// Get front element reference.
    pragma(inline, true)
    scope ref inout(T) front() inout return @property
    {
        // TODO use?: enforce(!empty); emsi-containers doesn't, std.container.Array does
        return slice()[0];
    }

    /// Get back element reference.
    pragma(inline, true)
    scope ref inout(T) back() inout return @property
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
            insertBackMove(*cast(MutableE*)(&value));
        }
        _length += 1;
    }

    /** Insert unmoveable `value` into the end of the array.
     */
    pragma(inline, true)
    void insertBack()(T value) @trusted
        if (!isCopyable!T)
    {
        insertBackMove(value);
    }

    /** Insert the elements `values` into the end of the array.
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

    /** Insert the elements `values` into the end of the array.
     */
    void insertBack(R)(R values)
        if (isAssignableFromElementsOfRefIterableStruct!R)
    {
        import std.range : hasLength;
        static if (isInputRange!R &&
                   hasLength!R)
        {
            reserve(_length + values.length);
            import std.algorithm : copy;
            copy(values, _mptr[_length .. _length + values.length]);
            _length += values.length;
        }
        else
        {
            foreach (ref value; move(values)) // TODO remove `move` when compiler does it for us
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

    /// ditto
    alias put = insertBack;

    /** Remove last value fromm the end of the array.
     */
    void popBack()              // TODO name it `removeBack` instead?
    {
        // import std.exception : enforce;
        // enforce(!empty);        // TODO use `assert` instead?
        assert(!empty);
        _length -= 1;
        static if (hasElaborateDestructor!T)
        {
            .destroy(_mptr[_length]);
        }
    }

    /** Pop back element and return it. */
    T backPop() @trusted
    {
        assert(!empty);
        _length -= 1;
        static if (needsMove!T)
        {
            return move(_mptr[_length]); // move is indeed need here
        }
        else
        {
            return _mptr[_length]; // no move needed
        }
    }
    alias stealBack = backPop;

    /** Removal doesn't need to care about ordering. */
    T removeAtIndex(size_t index)
        @trusted
        @("complexity", "O(length)")
    {
        assert(index < this.length);
        auto value = move(_mptr[index]);

        // TODO use this instead:
        // immutable si = index + 1;   // source index
        // immutable ti = index;       // target index
        // immutable restLength = this.length - (index + 1);
        // moveEmplaceAll(_mptr[si .. si + restLength],
        //                _mptr[ti .. ti + restLength]);

        foreach (immutable i; 0 .. this.length - (index + 1)) // each element index that needs to be moved
        {
            immutable si = index + i + 1; // source index
            immutable ti = index + i; // target index
            moveEmplace(_mptr[si], // TODO remove `move` when compiler does it for us
                        _mptr[ti]);
        }

        _length -= 1;
        return move(value); // TODO remove `move` when compiler does it for us
    }

    pragma(inline, true)
    T popFront()
        @trusted
        @("complexity", "O(length)")
    {
        return removeAtIndex(0);
    }

    /** Forwards to $(D insertBack(values)).
     */
    pragma(inline, true)
    void opOpAssign(string op)(T value)
        if (op == "~")
    {
        insertBackMove(value);
    }

    /// ditto
    pragma(inline, true)
    void opOpAssign(string op, U)(U[] values...) @trusted
        if (op == "~" &&
            isElementAssignable!U &&
            isCopyable!U)       // prevent accidental move of l-value `values`
    {
        insertBack(values);
    }

    /// ditto
    pragma(inline, true)
    void opOpAssign(string op, R)(R values)
        if (op == "~" &&
            isInputRange!R &&
            !isInfinite!R &&
            !isArray!R &&
            isElementAssignable!(ElementType!R))
    {
        insertBack(values);
    }

    // typeof(this) opBinary(string op, R)(R values)
    //     if (op == "~")
    // {
    //     // TODO: optimize
    //     typeof(this) result;
    //     result ~= this[];
    //     assert(result.length == length);
    //     result ~= values[];
    //     return result;
    // }

    /// Helper slice.
    pragma(inline, true)
    scope private inout(T)[] slice() inout return  @trusted
    {
        return _ptr[0 .. _length];
    }

    /// Helper mutable slice.
    pragma(inline, true)
    scope private MutableE[] mslice() return  @trusted
    {
        return _mptr[0 .. _length];
    }

    /// Reallocate storage.
    private void reallocateAndSetCapacity(size_t newCapacity) @trusted
    {
        _capacity = newCapacity;
        _ptr = cast(T*)realloc(_mptr, T.sizeof * _capacity);
        assert(_mptr, "Reallocation failed");
    }

    /// Mutable pointer.
    pragma(inline, true)
    scope private MutableE* _mptr() const return  @trusted
    {
        return cast(typeof(return))_ptr;
    }

private:
    // defined here https://dlang.org/phobos/std_experimental_allocator_gc_allocator.html#.GCAllocator
    static if (is(Allocator == std.experimental.allocator.gc_allocator.GCAllocator))
    {
        T* _ptr;                // GC-allocated store pointer
    }
    else
    {
        @nogc T* _ptr;          // non-GC-allocated store pointer
    }
    size_t _capacity;           // store capacity
    size_t _length;             // store length
}

/// construct and append from slices
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = CopyableArray!(T);

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
    alias A = CopyableArray!(T);

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
    alias T = int;
    alias A = CopyableArray!(T);

    A a;                        // default construction allowed
    assert(a.empty);
    assert(a.length == 0);
    assert(a.capacity == 0);
    assert(a[] == []);

    auto b = CopyableArray!int.withLength(3);
    assert(!b.empty);
    assert(b.length == 3);
    assert(b.capacity == 3);
    b[0] = 1;
    b[1] = 2;
    b[2] = 3;
    assert(b[] == [1, 2, 3].s);

    b[] = [4, 5, 6].s;
    assert(b[] == [4, 5, 6].s);

    const c = CopyableArray!int.withCapacity(3);
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

    const e = CopyableArray!int([1, 2, 3, 4].s);
    assert(e.length == 4);
    assert(e[] == [1, 2, 3, 4].s);
}

@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = CopyableArray!(T);

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

/// scope checking
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = CopyableArray!T;

    scope T[] leakSlice() @safe return
    {
        A a;
        return a[];             // TODO shouldn't compile with -dip1000
    }

    scope T* leakPointer() @safe return
    {
        A a;
        return a._ptr;          // TODO shouldn't compile with -dip1000
    }

    auto lp = leakPointer();    // TODO shouldn't compile with -dip1000
    auto ls = leakSlice();      // TODO shouldn't compile with -dip1000
    T[] as = A(1, 2)[];         // TODO shouldn't compile with -dip1000
    auto bs = A(1, 2)[];        // TODO shouldn't compile with -dip1000
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

/// construct and insert from non-copyable element type passed by value
@safe pure nothrow /*@nogc*/ unittest
{
    alias A = CopyableArray!(SomeUncopyableStruct);

    A a = A(SomeUncopyableStruct(17));
    assert(a[] == [SomeUncopyableStruct(17)]);

    a.insertBack(SomeUncopyableStruct(18));
    assert(a[] == [SomeUncopyableStruct(17),
                   SomeUncopyableStruct(18)]);

    a ~= SomeUncopyableStruct(19);
    assert(a[] == [SomeUncopyableStruct(17),
                   SomeUncopyableStruct(18),
                   SomeUncopyableStruct(19)]);
}

/// construct from slice of uncopyable type
@safe pure nothrow @nogc unittest
{
    alias A = CopyableArray!(SomeUncopyableStruct);
    // TODO can we safely support this?: A a = [SomeUncopyableStruct(17)];
}

// construct from array with uncopyable elements
@safe pure nothrow @nogc unittest
{
    alias A = CopyableArray!(SomeUncopyableStruct);

    A a;
    assert(a.empty);

    a.insertBack(A.init);
    assert(a.empty);
}

// construct from range of uncopyable elements
@safe pure nothrow @nogc unittest
{
    alias T = SomeUncopyableStruct;
    alias A = CopyableArray!T;

    A a;
    assert(a.empty);

    import std.algorithm : map, filter;

    const b = A([10, 20, 30].s[].map!(_ => T(_^^2))); // hasLength
    assert(b.length == 3);
    assert(b == [T(100), T(400), T(900)].s);

    const c = A([10, 20, 30].s[].filter!(_ => _ == 30).map!(_ => T(_^^2))); // !hasLength
    assert(c == [T(900)].s);
}

/// construct with string as element type that needs GC-range
@safe pure nothrow @nogc unittest
{
    alias T = string;
    alias A = CopyableArray!(T);

    A a;
    a ~= `alpha`;
    a ~= `beta`;
    a ~= [`gamma`, `delta`].s;
    assert(a[] == [`alpha`, `beta`, `gamma`, `delta`].s);

    const b = [`epsilon`].s;

    a.insertBack(b);
    assert(a[] == [`alpha`, `beta`, `gamma`, `delta`, `epsilon`].s);

    a ~= b;
    assert(a[] == [`alpha`, `beta`, `gamma`, `delta`, `epsilon`, `epsilon`].s);
}

/// convert to string
unittest
{
    // import arrayn : ArrayN;
    // alias Chars7 = ArrayN!(char, 7);
    // alias A = CopyableArray!(Chars7);
    // CopyableArray!char sink;
    // TODO make this work: A([1, 2, 3]).toString(sink.put);
}

/// convert to string
unittest
{
    alias T = int;
    alias A = CopyableArray!(T);
    CopyableArray!char sink;
    // TODO make this work: A([1, 2, 3]).toString(sink.put);
}

/// foreach
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = CopyableArray!(T);
    auto a = A([1, 2, 3].s);
    foreach (const i, const e; a)
    {
        assert(i + 1 == e);
    }
}

/// TODO Move to Phobos.
private enum bool isRefIterable(T) = is(typeof({ foreach (ref elem; T.init) {} }));

version(unittest)
{
    import array_help : s;
}
