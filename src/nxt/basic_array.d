module nxt.basic_array;

import core.internal.traits : Unqual;

@safe:

/** Array type with deterministic control of memory. The memory allocated for
 * the array is reclaimed as soon as possible; there is no reliance on the
 * garbage collector. Array uses malloc, realloc and free for managing its own
 * memory.
 *
 * Use `std.bitmanip.BitArray` for array container storing boolean values.
 *
 * TODO optimize by making members templates. 0.579s before, eval-dwim: 0.67s
 *
 * TODO Add OutputRange.writer support as
 * https://github.com/burner/StringBuffer/blob/master/source/stringbuffer.d#L45
 *
 * TODO Use `std.traits.areCopyCompatibleArrays`
 *
 * See_Also: https://github.com/facebook/folly/blob/master/folly/docs/FBVector.md
 */
struct BasicArray(T,
                  alias Allocator = null, // null means means to qcmeman functions. TODO use `PureMallocator` by default
                  CapacityType = size_t)  // see also https://github.com/izabera/s
if (!is(Unqual!T == bool) &&             // use `BitArray` instead
    (is(CapacityType == ulong) ||        // 3 64-bit words
     is(CapacityType == uint)))          // 2 64-bit words
{
@safe:

    // import core.exception : onOutOfMemoryError;
    import core.internal.traits : hasElaborateDestructor;

    import std.range.primitives : isInputRange, ElementType, isInfinite;
    import std.traits : hasIndirections, hasAliasing,
        isMutable, TemplateOf, isArray, isAssignable, isCopyable, isType, hasFunctionAttributes, isIterable;
    import core.lifetime : emplace, move, moveEmplace;

    import nxt.qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;
    import nxt.container_traits : mustAddGCRange, needsMove, isAddress;

    /// Mutable element type.
    private alias MutableE = Unqual!T;

    /// Is `true` if `U` can be assign to the element type `T` of `this`.
    enum isElementAssignable(U) = isAssignable!(MutableE, U);

pragma(inline):

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    static typeof(this) withLength()(size_t initialLength) // template-lazy
    {
        pragma(inline, true);
        return withCapacityLengthZero(initialLength, initialLength, true);
    }

    /// Returns: an array with initial capacity `initialCapacity`.
    static typeof(this) withCapacity()(size_t initialCapacity) // template-lazy
    {
        pragma(inline, true);
        return withCapacityLengthZero(initialCapacity, 0, false);
    }

    static if (isCopyable!T)
    {
        /** Construct using
         * - initial length `length`,
         * - and value of all elements `elementValue`.
         */
        static typeof(this) withLengthElementValue()(size_t length,
                                                     T elementValue)
        {
            pragma(inline, true);
            assert(length <= CapacityType.max);
            return typeof(return)(Store(typeof(this).allocateWithValue(length, move(elementValue)),
                                        cast(CapacityType)length,
                                        cast(CapacityType)length));
        }
    }

    /** Construct using
     * - initial capacity `capacity`,
     * - initial length `length`,
     * - and zeroing-flag `zero`.
     */
    private static typeof(this) withCapacityLengthZero()(size_t capacity, // template-lazy
                                                         size_t length,
                                                         bool zero) @trusted
    {
        version(LDC) pragma(inline, true);
        assert(capacity >= length);
        assert(capacity <= CapacityType.max);
        return typeof(return)(Store(typeof(this).allocate(capacity, zero),
                                    cast(CapacityType)capacity,
                                    cast(CapacityType)length));
    }

    /** Emplace `thatPtr` with elements moved from `elements`. */
    static ref typeof(this) emplaceWithMovedElements()(typeof(this)* thatPtr, // template-lazy
                                                       T[] elements) @system
    {
        immutable length = elements.length;
        thatPtr._store.ptr = typeof(this).allocate(length, false);
        thatPtr._store.capacity = cast(CapacityType)length;
        thatPtr._store.length = cast(CapacityType)length;
        foreach (immutable i, ref e; elements[])
        {
            moveEmplace(e, thatPtr._mptr[i]);
        }
        return *thatPtr;
    }

    /** Emplace `thatPtr` with elements copied from `elements`. */
    static ref typeof(this) emplaceWithCopiedElements()(typeof(this)* thatPtr, // template-lazy
                                                        const(T)[] elements) @system
    if (isCopyable!T)
    {
        immutable length = elements.length;
        thatPtr._store.ptr = typeof(this).allocate(length, false);
        thatPtr._store.capacity = cast(CapacityType)length;
        thatPtr._store.length = cast(CapacityType)length;
        foreach (immutable i, ref e; elements[])
        {
            thatPtr._mptr[i] = cast(T)e; // TODO restrict this function using a
                                         // T-trait where this cast can be @trusted
        }
        return *thatPtr;
    }

    private this(Store store)
    {
        _store = store;
    }

    /// Construct from uncopyable element `value`.
    this()(T value) @trusted    // template-lazy
    if (!isCopyable!T)
    {
        _store.ptr = typeof(this).allocate(1, false);
        _store.capacity = 1;
        _store.length = 1;
        moveEmplace(value, _mptr[0]); // TODO remove `moveEmplace` when compiler does it for us
    }

    /// Construct from copyable element `value`.
    this(U)(U value) @trusted
    if (isCopyable!U &&
        isElementAssignable!U)
    {
        _store.ptr = typeof(this).allocate(1, false);
        _store.capacity = 1;
        _store.length = 1;
        emplace(&_mptr[0], value);
    }

    static if (isCopyable!T &&
               !is(T == union)) // forbid copying of unions such as `HybridBin` in hashmap.d
    {
        static typeof(this) withElements()(const T[] elements) @trusted // template-lazy
        {
            immutable length = elements.length;
            auto ptr = typeof(this).allocate(length, false);

            foreach (immutable i, const element; elements[])
            {
                // TODO: be more strict
                // static if (hasIndirections!T)
                // {
                //     ptr[i] = element;
                // }
                // else
                // {
                //     ptr[i] = *cast(MutableE*)&element;
                // }
                ptr[i] = *cast(MutableE*)&element;
            }

            // ptr[0 .. length] = elements[];
            return typeof(return)(Store(ptr,
                                        cast(CapacityType)length,
                                        cast(CapacityType)length));
        }

        /// Returns: shallow duplicate of `this`.
        @property BasicArray!(Unqual!T, Allocator, CapacityType) dup()() const @trusted // template-lazy
        {
            pragma(inline, true);
            return typeof(this).withElements(this[]);
        }
    }


    /// Construct from element(s) `values`.
    this(U)(U[] values...) @trusted
    if (isCopyable!U &&
        isElementAssignable!U) // prevent accidental move of l-value `values` in array calls
    {
        if (values.length == 1) // TODO branch should be detected at compile-time
        {
            // twice as fast as array assignment below
            _store.ptr = typeof(this).allocate(1, false);
            _store.capacity = 1;
            _store.length = 1;
            emplace(&_mptr[0], values[0]);
            return;
        }
        reserve(values.length);
        _store.length = cast(CapacityType)values.length;
        import nxt.emplace_all : moveEmplaceAllNoReset;
        moveEmplaceAllNoReset(values, _mptr[0 .. _store.length]);
    }

    /// Construct from `n` number of element(s) `values` (in a static array).
    this(uint n)(T[n] values...) @trusted
    {
        reserve(values.length);
        _store.length = cast(CapacityType)values.length;
        // TODO use import emplace_all instead
        static foreach (i; 0 .. values.length)
        {
            _mptr[i] = values[i];
        }
    }

    /** Is `true` iff constructable from the iterable (or range) `I`.
     */
    private enum isAssignableFromElementsOfFiniteRefIterable(I) = (is(I == struct) && // exclude class ranges for aliasing control
                                                                   isRefIterable!I && // elements may be non-copyable
                                                                   !isInfinite!I &&
                                                                   isElementAssignable!(ElementType!I));

    /// Construct from the elements `values`.
    static typeof(this) withElementsOfRange_untested(R)(R values) @trusted
    if (isAssignableFromElementsOfFiniteRefIterable!R)
    {
        import std.range.primitives : hasLength, hasSlicing;

        typeof(this) result;

        static if (hasLength!R &&
                   hasSlicing!R &&
                   isCopyable!(ElementType!R) &&
                   !hasElaborateDestructor!(ElementType!R))
        {
            result.reserve(values.length);
            import std.algorithm.mutation : copy;
            copy(values[0 .. values.length],
                 result._mptr[0 .. values.length]); // TODO better to use foreach instead?
            result._store.length = values.length;
        }
        else
        {
            static if (hasLength!R)
            {
                result.reserve(values.length);
                size_t i = 0;
                foreach (ref value; move(values)) // TODO remove `move` when compiler does it for us
                {
                    static if (needsMove!(typeof(value)))
                    {
                        moveEmplace(value, result._mptr[i++]);
                    }
                    else
                    {
                        result._mptr[i++] = value;
                    }
                }
                result._store.length = values.length;
            }
            else
            {
                // import std.algorithm.mutation : moveEmplaceAll;
                /* TODO optimize with `moveEmplaceAll` that does a raw copy and
                 * zeroing of values */
                foreach (ref value; move(values)) // TODO remove `move` when compiler does it for us
                {
                    static if (needsMove!(ElementType!R))
                    {
                        result.insertBackMove(value); // steal element
                    }
                    else
                    {
                        result.insertBack1(value);
                    }
                }
            }
        }
        return result;
    }

    /// No default copying.
    @disable this(this);

    // TODO: this gives error in insertBack. why?
    // void opAssign()(typeof(this) rhs) @trusted pure nothrow @nogc // template-lazy
    // {
    //     move(rhs, this);
    // }

    /** Destruct.
     *
     * TODO what effect does have here?
     * See_Also: https://github.com/atilaneves/automem/blob/master/source/automem/vector.d#L92
     */
    ~this() /*TODO scope*/
    {
        release();
    }

    /// Empty.
    void clear()
    {
        release();
        resetInternalData();
    }

    /// Release internal store.
    private void release()
    {
        static if (hasElaborateDestructor!T)
        {
            destroyElements();
        }
        freeStore();
    }

    /// Destroy elements.
    static if (hasElaborateDestructor!T)
    {
        private void destroyElements() @trusted
        {
            foreach (immutable index; 0 .. _store.length)
            {
                .destroy(_mptr[index]);
            }
        }
    }

    /// Free internal store.
    private void freeStore() @trusted
    {
        static if (mustAddGCRange!T)
        {
            gc_removeRange(_mptr);
        }
        free(_mptr);
    }

    /// Reset internal data.
    private void resetInternalData()
    {
        pragma(inline, true);
        _store.ptr = null;
        _store.capacity = 0;
        _store.length = 0;
    }

    /** Allocate heap region with `initialCapacity` number of elements of type `T`.
     *
     * If `zero` is `true` they will be zero-initialized.
     */
    private static MutableE* allocate(size_t initialCapacity, bool zero) @trusted
    {
        immutable size_t numBytes = initialCapacity * T.sizeof;

        typeof(return) ptr = null;
        static if (!is(typeof(Allocator) == typeof(null)))
        {
            if (zero)
            {
                import std.experimental.allocator : makeArray;
                ptr = Allocator.makeArray!T(initialCapacity, 0).ptr; // TODO set length
            }
            else
            {
                ptr = cast(typeof(return))Allocator.allocate(numBytes).ptr; // TODo set length
            }
        }
        else
        {
            if (zero)
            {
                ptr = cast(typeof(return))calloc(initialCapacity, T.sizeof);
            }
            else
            {
                ptr = cast(typeof(return))malloc(numBytes);
            }
            assert(ptr, "Allocation failed");
        }

        if (ptr is null && initialCapacity >= 1 )
        {
            // onOutOfMemoryError();
            return null;
        }

        static if (mustAddGCRange!T)
        {
            gc_addRange(ptr, numBytes);
        }

        return ptr;
    }

    static if (isCopyable!T)
    {
        /** Allocate heap region with `initialCapacity` number of elements of type `T` all set to `elementValue`.
         */
        private static MutableE* allocateWithValue(size_t initialCapacity,
                                                   T elementValue) @trusted
        {
            immutable size_t numBytes = initialCapacity * T.sizeof;

            typeof(return) ptr = null;
            static if (!is(typeof(Allocator) == typeof(null)))
            {
                import std.experimental.allocator : makeArray;
                ptr = Allocator.makeArray!T(initialCapacity, elementValue).ptr; // TODO set length
                if (ptr is null && initialCapacity >= 1)
                {
                    // onOutOfMemoryError();
                    return null;
                }
            }
            else
            {
                ptr = cast(typeof(return))malloc(numBytes);
                if (ptr is null && initialCapacity >= 1)
                {
                    // onOutOfMemoryError();
                    return null;
                }
                foreach (immutable index; 0 .. initialCapacity)
                {
                    emplace(&ptr[index], elementValue);
                }
            }

            static if (mustAddGCRange!T)
            {
                gc_addRange(ptr, numBytes);
            }

            return ptr;
        }
    }

    /** Comparison for equality. */
    bool opEquals()(const scope auto ref typeof(this) rhs) const scope // template-lazy
    {
        pragma(inline, true);
        return slice() == rhs.slice();
    }
    /// ditto
    bool opEquals(U)(const scope U[] rhs) const scope
    if (is(typeof(T[].init == U[].init)))
    {
        pragma(inline, true);
        return slice() == rhs;
    }

    /// Calculate D associative array (AA) key hash.
    hash_t toHash()() const scope @trusted // template-lazy
    {
        import core.internal.hash : hashOf;
        static if (isCopyable!T)
        {
            return this.length ^ hashOf(slice());
        }
        else
        {
            typeof(return) hash = this.length;
            foreach (immutable index; 0 .. this.length)
            {
                hash ^= this.ptr[index].hashOf;
            }
            return hash;
        }
    }

    static if (isCopyable!T)
    {
        /** Construct a string representation of `this` at `sink`.
         */
        void toString()(scope void delegate(scope const(char)[]) sink) const scope // template-lazy
        {
            sink("[");
            foreach (immutable ix, ref value; slice())
            {
                import std.format : formattedWrite;
                sink.formattedWrite("%s", value);
                if (ix + 1 < length) { sink(", "); } // separator
            }
            sink("]");
        }
    }

    /// Check if empty.
    @property bool empty()() const scope // template-lazy
    {
        pragma(inline, true);
        return _store.length == 0;
    }

    /// Get length.
    @property size_t length() const scope // can't be template-lazy
    {
        pragma(inline, true)
        return _store.length;
    }
    alias opDollar = length;    /// ditto

    /** Set length to `newLength`.
     *
     * If `newLength` < `length` elements are truncate.
     * If `newLength` > `length` default-initialized elements are appended.
     */
    @property void length(size_t newLength) @trusted scope // can't template-lazy
    {
        if (newLength < length) // if truncatation
        {
            static if (hasElaborateDestructor!T)
            {
                foreach (immutable index; newLength .. _store.length)
                {
                    .destroy(_mptr[index]);
                }
            }
            else static if (isAddress!T)
            {
                foreach (immutable index; newLength .. _store.length)
                {
                    _mptr[index] = null;
                }
            }
        }
        else
        {
            reserve(newLength);
            static if (hasElaborateDestructor!T)
            {
                // TODO remove when compiler does it for us
                foreach (immutable index; _store.length .. newLength)
                {
                    // TODO remove when compiler does it for us:
                    static if (isCopyable!T)
                    {
                        emplace(&_mptr[index], T.init);
                    }
                    else
                    {
                        auto _ = T.init;
                        moveEmplace(_, _mptr[index]);
                    }
                }
            }
            else
            {
                _mptr[_store.length .. newLength] = T.init;
            }
        }

        assert(newLength <= CapacityType.max);
        _store.length = cast(CapacityType)newLength;
    }

    /// Get capacity.
    @property size_t capacity() const scope // can't be template-lazy
    {
        pragma(inline, true)
        return _store.capacity;
    }

    /** Ensures sufficient capacity to accommodate for minimumCapacity number
     * of elements. If `minimumCapacity` < `capacity`, this method does
     * nothing.
     */
    void reserve(size_t minimumCapacity) @trusted scope pure nothrow @nogc
    {
        static if (!is(CapacityType == size_t))
        {
            assert(minimumCapacity <= CapacityType.max);
        }

        if (minimumCapacity <= capacity) { return; }

        // growth factor
        // Motivation: https://github.com/facebook/folly/blob/master/folly/docs/FBVector.md#memory-handling
        reallocateAndSetCapacity(3*minimumCapacity/2); // use 1.5 like Facebook's `fbvector` does

        // import std.math : nextPow2;
        // reallocateAndSetCapacity(minimumCapacity.nextPow2);
    }

    /// Index support.
    ref inout(T) opIndex()(size_t i) inout return // template-lazy
    {
        pragma(inline, true);
        return slice()[i];
    }

    /// Slice support.
    inout(T)[] opSlice()(size_t i, size_t j) inout return // template-lazy
    {
        pragma(inline, true);
        return slice()[i .. j];
    }
    /// ditto
    inout(T)[] opSlice()() inout return // template-lazy
    {
        pragma(inline, true);
        return slice();
    }

    /// Index assignment support.
    ref T opIndexAssign(U)(scope U value, size_t i) @trusted return
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
    T[] opSliceAssign(U)(scope U value) return
    {
        pragma(inline, true);
        return slice()[] = value;
    }

    /// ditto
    T[] opSliceAssign(U)(scope U value, size_t i, size_t j) return
    {
        pragma(inline, true);
        return slice()[i .. j] = value;
    }

    /// Get reference to front element.
    ref inout(T) front()() inout return @property // template-lazy
    {
        pragma(inline, true);
        return slice()[0];      // range-checked by default
    }

    /// Get reference to back element.
    ref inout(T) back()() inout return @property // template-lazy
    {
        pragma(inline, true);
        return slice()[_store.length - 1]; // range-checked by default

    }

    /** Move `value` into the end of the array.
     */
    void insertBackMove()(ref T value) @trusted // template-lazy
    {
        version(LDC) pragma(inline, true);
        reserve(_store.length + 1);
        moveEmplace(value, _mptr[_store.length]);
        _store.length += 1;
    }

    /** Insert unmoveable `value` into the end of the array.
     */
    void insertBack()(T value) @trusted // template-lazy
    if (!isCopyable!T)
    {
        version(LDC) pragma(inline, true);
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
            reserve(_store.length + values.length);
            _mptr[_store.length .. _store.length + values.length] = values;
        }
        else
        {
            import nxt.overlapping : overlaps;
            if (_store.ptr == values.ptr) // called for instances as: `this ~= this`
            {
                reserve(2*_store.length); // invalidates `values.ptr`
                foreach (immutable i; 0 .. _store.length)
                {
                    _mptr[_store.length + i] = _store.ptr[i];
                }
            }
            else if (overlaps(this[], values[]))
            {
                assert(0, `TODO Handle overlapping arrays`);
            }
            else
            {
                reserve(_store.length + values.length);
                _mptr[_store.length .. _store.length + values.length] = values;
            }
        }
        _store.length += values.length;
    }

    /** Insert `value` into the end of the array.
     *
     * TODO rename to `insertBack` and make this steal scalar calls over
     * insertBack(U)(U[] values...) overload below
     */
    void insertBack1()(T value) @trusted // template-lazy
    {
        reserve(_store.length + 1);
        static if (needsMove!T)
        {
            insertBackMove(*cast(MutableE*)(&value));
        }
        else
        {
            _mptr[_store.length] = value;
        }
        _store.length += 1;
    }

    /** Insert the elements `elements` into the end of the array.
     */
    void insertBack(R)(scope R elements) @trusted
    if (isAssignableFromElementsOfFiniteRefIterable!R)
    {
        import std.range.primitives : hasLength;
        static if (isInputRange!R &&
                   hasLength!R)
        {
            reserve(_store.length + elements.length);
            import std.algorithm.mutation : copy;
            copy(elements, _mptr[_store.length .. _store.length + elements.length]);
            _store.length += elements.length;
        }
        else
        {
            foreach (ref element; move(elements)) // TODO remove `move` when compiler does it for us
            {
                static if (isCopyable!(ElementType!R))
                {
                    insertBack(element);
                }
                else
                {
                    insertBackMove(element);
                }
            }
        }
    }

    /// ditto
    alias put = insertBack;

    /** Remove last value fromm the end of the array.
     */
    void popBack()() @trusted   // template-lazy
    {
        pragma(inline, true);
        assert(!empty);
        _store.length -= 1;
        static if (hasElaborateDestructor!T)
        {
            .destroy(_mptr[_store.length]);
        }
        else static if (isAddress!T)
        {
            _mptr[_store.length] = null;
        }
    }

    /** Rmove `n` last values from the end of the array.
     *
     * See_Also: http://mir-algorithm.libmir.org/mir_appender.html#.ScopedBuffer.popBackN
     */
    void popBackN()(size_t n) @trusted   // template-lazy
    {
        assert(length >= n);
        _store.length -= n;
        static if (hasElaborateDestructor!T)
        {
            foreach (const index ; 0 .. n)
            {
                .destroy(_mptr[_store.length + index]);
            }
        }
        else static if (isAddress!T)
        {
            foreach (const index ; 0 .. n)
            {
                _mptr[_store.length + index] = null;
            }
        }
    }

    /** Pop back element and return it. */
    T backPop()() @trusted      // template-lazy
    {
        pragma(inline, true);
        assert(!empty);
        _store.length -= 1;
        static if (needsMove!T)
        {
            return move(_mptr[_store.length]); // move is indeed need here
        }
        else
        {
            return _mptr[_store.length]; // no move needed
        }
    }

    /** Pop element at `index`. */
    void popAt()(size_t index) @trusted // template-lazy
        @("complexity", "O(length)")
    {
        assert(index < this.length);
        static if (hasElaborateDestructor!T)
        {
            .destroy(_mptr[index]);
        }
        else static if (isAddress!T)
        {
            _mptr[index] = null; // please the GC
        }
        shiftToFrontAt(index);
        _store.length -= 1;
    }

    /** Move element at `index` to return. */
    T moveAt()(size_t index) @trusted // template-lazy
        @("complexity", "O(length)")
    {
        assert(index < this.length);
        auto value = move(_mptr[index]);
        shiftToFrontAt(index);
        _store.length -= 1;
        return move(value); // TODO remove `move` when compiler does it for us
    }

    /** Move element at front. */
    T frontPop()()              // template-lazy
        @("complexity", "O(length)")
    {
        pragma(inline, true);
        return moveAt(0);
    }

    private void shiftToFrontAt()(size_t index) @trusted // template-lazy
    {
        // TODO use this instead:
        // immutable si = index + 1;   // source index
        // immutable ti = index;       // target index
        // immutable restLength = this.length - (index + 1);
        // import std.algorithm.mutation : moveEmplaceAll;
        // moveEmplaceAll(_mptr[si .. si + restLength],
        //                _mptr[ti .. ti + restLength]);
        foreach (immutable i; 0 .. this.length - (index + 1)) // each element index that needs to be moved
        {
            immutable si = index + i + 1; // source index
            immutable ti = index + i; // target index
            moveEmplace(_mptr[si], // TODO remove `move` when compiler does it for us
                        _mptr[ti]);
        }
    }

    /** Forwards to $(D insertBack(values)).
     */
    void opOpAssign(string op)(T value)
    if (op == "~")
    {
        pragma(inline, true);
        insertBackMove(value);
    }

    /// ditto
    void opOpAssign(string op, U)(U[] values...) @trusted
    if (op == "~" &&
        isElementAssignable!U &&
        isCopyable!U)       // prevent accidental move of l-value `values`
    {
        pragma(inline, true);
        insertBack(values);
    }

    /// ditto
    void opOpAssign(string op, R)(R values)
    if (op == "~" &&
        isInputRange!R &&
        !isInfinite!R &&
        !isArray!R &&
        isElementAssignable!(ElementType!R))
    {
        pragma(inline, true);
        insertBack(values);
    }

    void opOpAssign(string op)(auto ref typeof(this) values)
    if (op == "~")
    {
        pragma(inline, true);
        insertBack(values[]);
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
    private inout(T)[] slice() inout return @trusted
    {
        pragma(inline, true);
        return _store.ptr[0 .. _store.length];
    }

    /// Unsafe access to pointer.
    inout(T)* ptr()() inout return @system // template-lazy
    {
        pragma(inline, true);
        return _store.ptr;
    }

    /// Reallocate storage.
    private void reallocateAndSetCapacity()(size_t newCapacity) @trusted // template-lazy
    {
        assert(newCapacity <= CapacityType.max);

        static if (mustAddGCRange!T)
        {
            gc_removeRange(_store.ptr);
        }

        _store.capacity = cast(CapacityType)newCapacity;
        _store.ptr = cast(T*)realloc(_mptr, T.sizeof * _store.capacity);
        if (_store.ptr is null && newCapacity >= 1)
        {
            // onOutOfMemoryError();
            return;
        }

        static if (mustAddGCRange!T)
        {
            gc_addRange(_store.ptr, _store.capacity * T.sizeof);
        }
    }

    /// Mutable pointer.
    private MutableE* _mptr() const return @trusted
    {
        pragma(inline, true);
        return cast(typeof(return))_store.ptr;
    }

private:
    /** For more convenient construction. */
    struct Store
    {
        static if (!is(typeof(Allocator) == typeof(null)) &&
                   !hasFunctionAttributes!(Allocator.allocate, "@nogc"))
        {
            T* ptr;             // GC-allocated store pointer
        }
        else
        {
            import nxt.gc_traits : NoGc;
            @NoGc T* ptr;       // non-GC-allocated store pointer
        }

        CapacityType capacity; // store capacity
        CapacityType length;   // store length
    }

    Store _store;
}

import std.traits : isInstanceOf;
import std.functional : unaryFun;

/** Remove all elements matching `predicate`.
 *
 * Returns: number of elements that were removed.
 *
 * TODO implement version that doesn't use a temporary array `tmp`, which is
 * probably faster for small arrays.
 */
size_t remove(alias predicate, C)(ref C c) @trusted
    @("complexity", "O(length)")
if (isInstanceOf!(BasicArray, C) &&
    is(typeof(unaryFun!predicate(C.init[0]))))
{
    C tmp;
    size_t count = 0;
    foreach (immutable i; 0 .. c.length)
    {
        if (unaryFun!predicate(c[i]))
        {
            count += 1;
            import core.internal.traits : hasElaborateDestructor;
            import nxt.container_traits : isAddress;
            static if (hasElaborateDestructor!(typeof(c[i])))
            {
                .destroy(c[i]);
            }
            else static if (isAddress!(typeof(c[i])))
            {
                c[i] = null;    // please the GC
            }
        }
        else
        {
            tmp.insertBackMove(c[i]); // TODO remove unnecessary clearing of `_mptr[i]`
        }
    }

    c.freeStore();

    import core.lifetime : moveEmplace;
    moveEmplace(tmp, c);

    return count;
}

/// construct and append from slices
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!(T, null, uint);
    static if (size_t.sizeof == 8) // only 64-bit
    {
        static assert(A.sizeof == 2 * size_t.sizeof); // only two words
    }

    auto a = A([10, 11, 12].s);

    a ~= a[];
    assert(a[] == [10, 11, 12,
                   10, 11, 12].s);

    a ~= false;
    assert(a[] == [10, 11, 12,
                   10, 11, 12, 0].s);
}

///
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

    import std.algorithm.iteration : filter;

    a.insertBack([42].s[].filter!(_ => _ is 42));
    assert(a[] == [10, 11, 12, 0, 3, 42].s);

    a.insertBack([42].s[].filter!(_ => _ !is 42));
    assert(a[] == [10, 11, 12, 0, 3, 42].s);

    a ~= a[];
    assert(a[] == [10, 11, 12, 0, 3, 42,
                   10, 11, 12, 0, 3, 42].s);
}

///
@safe pure nothrow @nogc unittest
{
    alias T = int;
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

///
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!(T);

    auto a = A([1, 2, 3].s);
    A b = a.dup;                // copy construction enabled

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


/// DIP-1000 return ref escape analysis
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!T;

    T[] leakSlice() @safe pure nothrow @nogc
    {
        A a;
        return a[];             // TODO shouldn't compile with -dip1000
    }

    T* leakPointer() @safe pure nothrow @nogc
    {
        A a;
        return a._store.ptr;    // TODO shouldn't compile with -dip1000
    }

    auto lp = leakPointer();    // TODO shouldn't compile with -dip1000
    auto ls = leakSlice();      // TODO shouldn't compile with -dip1000
    T[] as = A(1, 2)[];         // TODO shouldn't compile with -dip1000
    auto bs = A(1, 2)[];        // TODO shouldn't compile with -dip1000
}

version(unittest)
{
    /// uncopyable struct
    private static struct SomeUncopyable
    {
        @disable this(this);
        int x;
    }
}

/// construct and insert from non-copyable element type passed by value
@safe pure nothrow /*@nogc*/ unittest
{
    alias A = BasicArray!(SomeUncopyable);

    A a = A(SomeUncopyable(17));
    assert(a[] == [SomeUncopyable(17)]);

    a.insertBack(SomeUncopyable(18));
    assert(a[] == [SomeUncopyable(17),
                   SomeUncopyable(18)]);

    a ~= SomeUncopyable(19);
    assert(a[] == [SomeUncopyable(17),
                   SomeUncopyable(18),
                   SomeUncopyable(19)]);
}

/// construct from slice of uncopyable type
@safe pure nothrow @nogc unittest
{
    alias A = BasicArray!(SomeUncopyable);
    // TODO can we safely support this?: A a = [SomeUncopyable(17)];
}

// construct from array with uncopyable elements
@safe pure nothrow @nogc unittest
{
    alias A = BasicArray!(SomeUncopyable);

    A a;
    assert(a.empty);

    a.insertBack(A.init);
    assert(a.empty);
}

// construct from ranges of uncopyable elements
@safe pure nothrow @nogc unittest
{
    alias T = SomeUncopyable;
    alias A = BasicArray!T;

    A a;
    assert(a.empty);

    import std.algorithm.iteration : map, filter;

    const b = A.withElementsOfRange_untested([10, 20, 30].s[].map!(_ => T(_^^2))); // hasLength
    assert(b.length == 3);
    assert(b == [T(100), T(400), T(900)].s);

    const c = A.withElementsOfRange_untested([10, 20, 30].s[].filter!(_ => _ == 30).map!(_ => T(_^^2))); // !hasLength
    const d = [T(900)].s;
    assert(c[] == d[]);
}

// construct from ranges of copyable elements
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!T;

    A a;
    assert(a.empty);

    import std.algorithm.iteration : map, filter;

    const b = A.withElementsOfRange_untested([10, 20, 30].s[].map!(_ => T(_^^2))); // hasLength
    assert(b.length == 3);
    assert(b == [T(100), T(400), T(900)].s);

    const c = A.withElementsOfRange_untested([10, 20, 30].s[].filter!(_ => _ == 30).map!(_ => T(_^^2))); // !hasLength
    assert(c == [T(900)].s);
}

/// construct with string as element type that needs GC-range
@safe pure nothrow @nogc unittest
{
    alias T = string;
    alias A = BasicArray!(T);

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
    alias T = int;
    alias A = BasicArray!(T);

    BasicArray!char sink;
    // TODO make this work: A([1, 2, 3]).toString(sink.put);
}

/// foreach
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!(T);

    auto a = A([1, 2, 3].s);

    foreach (immutable i, const e; a)
    {
        assert(i + 1 == e);
    }
}

/// removal
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!(T);

    auto a = A([1, 2, 3].s);
    assert(a == [1, 2, 3].s);

    assert(a.frontPop() == 1);
    assert(a == [2, 3].s);

    a.popAt(1);
    assert(a == [2].s);

    a.popAt(0);
    assert(a == []);

    a.insertBack(11);
    assert(a == [11].s);

    assert(a.backPop == 11);

    a.insertBack(17);
    assert(a == [17].s);
    a.popBack();
    assert(a.empty);

    a.insertBack([11, 12, 13, 14, 15].s[]);
    a.popAt(2);
    assert(a == [11, 12, 14, 15].s);
    a.popAt(0);
    assert(a == [12, 14, 15].s);
    a.popAt(2);

    assert(a == [12, 14].s);

    a ~= a;
}

/// removal
@safe pure nothrow unittest
{
    size_t mallocCount = 0;
    size_t freeCount = 0;

    struct S
    {
        @safe pure nothrow @nogc:

        alias E = int;

        import nxt.qcmeman : malloc, free;

        this(E x) @trusted
        {
            _ptr = cast(E*)malloc(E.sizeof);
            mallocCount += 1;
            *_ptr = x;
        }

        @disable this(this);

        ~this() @trusted
        {
            free(_ptr);
            freeCount += 1;
        }

        import nxt.gc_traits : NoGc;
        @NoGc E* _ptr;
    }

    /* D compilers cannot currently move stuff efficiently when using
     * std.algorithm.mutation.move. A final dtor call to the cleared sourced is
     * always done. */
    size_t extraDtor = 1;

    alias A = BasicArray!(S);
    static assert(!mustAddGCRange!A);
    alias AA = BasicArray!(A);
    static assert(!mustAddGCRange!AA);

    assert(mallocCount == 0);

    {
        A a;
        a.insertBack(S(11));
        assert(mallocCount == 1);
        assert(freeCount == extraDtor + 0);
    }

    assert(freeCount == extraDtor + 1);

    // assert(a.front !is S(11));
    // assert(a.back !is S(11));
    // a.insertBack(S(12));
}

/// test `OutputRange` behaviour with std.format
@safe pure /*TODO nothrow @nogc*/ unittest
{
    import std.format : formattedWrite;
    const x = "42";
    alias A = BasicArray!(char);
    A a;
    a.formattedWrite!("x : %s")(x);
    assert(a == "x : 42");
}

/// test emplaceWithMovedElements
@trusted pure nothrow @nogc unittest
{
    const x = "42";
    alias A = BasicArray!(char);

    auto ae = ['a', 'b'].s;

    A a = void;
    A.emplaceWithMovedElements(&a, ae[]);

    assert(a.length == ae.length);
    assert(a.capacity == ae.length);
    assert(a[] == ae);
}

/// test emplaceWithCopiedElements
@trusted pure nothrow @nogc unittest
{
    const x = "42";
    alias A = BasicArray!(char);

    auto ae = ['a', 'b'].s;

    A a = void;
    A.emplaceWithCopiedElements(&a, ae[]);

    assert(a.length == ae.length);
    assert(a.capacity == ae.length);
    assert(a[] == ae);
}

@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!(T, null, uint);
    const a = A(17);
    assert(a[] == [17].s);
}

/// check duplication via `dup`
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = BasicArray!(T);

    static assert(!__traits(compiles, { A b = a; })); // copying disabled

    auto a = A([10, 11, 12].s);
    auto b = a.dup;
    assert(a == b);
    assert(a[].ptr !is b[].ptr);
}

/// element type is a class
@safe pure nothrow unittest
{
    class T
    {
        this (int x)
        {
            this.x = x;
        }
        ~this() { x = 42; }
        int x;
    }
    alias A = BasicArray!(T);
    auto a = A([new T(10),
                new T(11),
                new T(12)].s);
    assert(a.length == 3);
    a.remove!(_ => _.x == 12);
    assert(a.length == 2);
}

/// check filtered removal via `remove`
@safe pure nothrow @nogc unittest
{
    struct T
    {
        int value;
    }

    alias A = BasicArray!(T);

    static assert(!__traits(compiles, { A b = a; })); // copying disabled

    auto a = A([T(10), T(11), T(12)].s);

    assert(a.remove!"a.value == 13" == 0);
    assert(a[] == [T(10), T(11), T(12)].s);

    assert(a.remove!"a.value >= 12" == 1);
    assert(a[] == [T(10), T(11)].s);

    assert(a.remove!(_ => _.value == 10) == 1);
    assert(a[] == [T(11)].s);

    assert(a.remove!(_ => _.value == 11) == 1);
    assert(a.empty);
}

/// construct from map range
@safe pure nothrow unittest
{
    import std.algorithm.iteration : map;
    alias T = int;
    alias A = BasicArray!(T);
    A a = A.withElementsOfRange_untested([10, 20, 30].s[].map!(_ => _^^2));
    assert(a[] == [100, 400, 900].s);
    a.popBackN(2);
    assert(a.length == 1);
    a.popBackN(1);
    assert(a.empty);
}

/// construct from map range
@trusted pure nothrow unittest
{
    alias T = int;
    alias A = BasicArray!(T);

    import std.typecons : RefCounted;
    RefCounted!A x;

    auto z = [1, 2, 3].s;
    x ~= z[];

    auto y = x;
    assert(y[] == z);

    auto _ = x.toHash;
}

/// GCAllocator
@trusted pure nothrow unittest
{
    import std.experimental.allocator.gc_allocator : GCAllocator;
    alias T = int;
    alias A = BasicArray!(T, GCAllocator.instance);
    A a;
}

/// construct with slices as element types
@trusted pure nothrow unittest
{
    alias A = BasicArray!(string);
    alias B = BasicArray!(char[]);
}

/** Variant of `BasicArray` with copy construction (postblit) enabled.
 *
 * See_Also: suppressing.d
 * See_Also: http://forum.dlang.org/post/eitlbtfbavdphbvplnrk@forum.dlang.org
 */
struct BasicCopyableArray
{
    /** TODO implement using instructions at:
     * http://forum.dlang.org/post/eitlbtfbavdphbvplnrk@forum.dlang.org
     */
}

/// TODO Move to Phobos.
private enum bool isRefIterable(T) = is(typeof({ foreach (ref elem; T.init) {} }));

version(unittest)
{
    import nxt.container_traits : mustAddGCRange, needsMove;
    import nxt.array_help : s;
    import nxt.dip_traits : isDIP1000;
}
