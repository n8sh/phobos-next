/** Array container(s) with optional sortedness via template-parameter
    `Ordering` and optional use of GC via `useGCAllocation`.

    TODO Use std.array.insertInPlace in insert()?
    TODO Use std.array.replaceInPlace?

    TODO Split up `Array` into `Array`, `SortedArray`, `SetArray` and reuse
    logic in `Array` via `alias this` or free functions.

    TODO Use `std.algorithm.mutation.move` and `std.range.primitives.moveAt`
    when moving internal sub-slices

    TODO struct Store, Notify andralex of packed array

    TODO Add `c.insertAfter(r, x)` where `c` is a collection, `r` is a range
    previously extracted from `c`, and `x` is a value convertible to
    collection's element type. See also:
    https://forum.dlang.org/post/n3qq6e$2bis$1@digitalmars.com

    TODO Document `isCopyable` in Phobos.
 */
module array_ex;

enum Ordering
{
    unsorted, // unsorted array
    sortedValues, // sorted array with possibly duplicate values
    sortedUniqueSet, // sorted array with unique values
}

enum IsOrdered(Ordering ordering) = ordering != Ordering.unsorted;

version(unittest)
{
    import dbgio : dln;
    import std.algorithm.comparison : equal;
    import std.meta : AliasSeq;
}

import container_traits : ContainerElementType;

/// Returns: `true` iff C is an `Array`.
import std.traits : isInstanceOf;
enum isMyArray(C) = isInstanceOf!(Array, C);

/// Semantics of copy construction and assignment.
enum Assignment
{
    disabled,           /// for reference counting use `std.typecons.RefCounted`
    move,               /// only move construction allowed
    copy                /// always copy (often not the desirable)
}

/** Array of value types `E` with optional ordering given by `ordering`.

    Params:
        useGCAllocation = `true` iff `GC.malloc` is used for store allocation,
                          otherwise C's `{m,ce,re}alloc()` is used.
 */
struct Array(E,
             Assignment assignment = Assignment.disabled,
             Ordering ordering = Ordering.unsorted,
             bool useGCAllocation = false,
             alias less = "a < b") // TODO move out of this definition and support only for the case when `ordering` is not `Ordering.unsorted`
{
    import std.range : isInputRange, ElementType;
    import std.traits : isAssignable, isCopyable, Unqual, isSomeChar, isArray;
    import std.functional : binaryFun;
    import std.meta : allSatisfy;
    import core.stdc.string : memset;
    import std.algorithm.mutation : move, moveEmplace;

    import qcmeman;

    static if (useGCAllocation)
    {
        import core.memory : GC;
    }
    static if (shouldAddGCRange!E)
    {
        import core.memory : GC;
    }

    /** Is `true` iff `T` is a type whose instances need to be scanned by the
        garbage collector (GC).
    */
    template shouldAddGCRange(T)
    {
        import std.traits : isPointer, hasIndirections, isInstanceOf;
        // pragma(msg, isInstanceOf!(Array, E)); // TODO why doesn't ever become true?
        enum shouldAddGCRange = (!isInstanceOf!(Array, E) &&
                                 (isPointer!T ||
                                  hasIndirections!T ||
                                  is (T == class)));
    }

    /// Type of element stored.
    // alias ElementType = E;

    /// Is `true` iff `Array` can be interpreted as a D `string`, `wstring` or `dstring`.
    enum isString = isSomeChar!E;

    alias comp = binaryFun!less; //< comparison

    /// Maximum number of elements that fits in SSO-packed
    enum smallLength = (_capacity.sizeof + _length.sizeof) / E.sizeof;

    /// Returns: `true` iff is SSO-packed.
    pragma(inline) bool isSmall() const @safe pure nothrow @nogc { return _length <= smallLength; }

    /// Create a empty array.
    this(typeof(null)) nothrow
    {
        // nothing needed, rely on default initialization of data members
    }

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    pragma(inline) static typeof(this) withLength(size_t initialLength) nothrow
    {
        typeof(return) that = void;
        that.allocateStoreWithCapacity(initialLength, true); // `true` here means zero initialize
        that._length = initialLength;
        // that.defaultInitialize();
        return that;
    }

    /// Returns: an array with initial capacity `initialCapacity`.
    pragma(inline) static typeof(this) withCapacity(size_t initialCapacity) nothrow
    {
        typeof(return) that = void;
        that.allocateStoreWithCapacity(initialCapacity);
        that._length = 0;
        return that;
    }

    /// Returns: an array of length 1 with first element set to `element`.
    pragma(inline) static typeof(this) withElement(E element) @trusted nothrow
    {
        typeof(return) that = void;
        that.allocateStoreWithCapacity(1);
        moveEmplace(element, that._ptr[0]);
        that._length = 1;
        return that;
    }

    // /// Returns: an array of length `Us.length` with elements set to `elements`.
    static typeof(this) withElements(Us...)(Us elements) @trusted nothrow
    {
        typeof(return) that = void;
        that.allocateStoreWithCapacity(Us.length);
        foreach (const i, ref element; elements)
        {
            moveEmplace(element, that._ptr[i]);
        }
        that._length = Us.length;
        return that;
    }

    static if (useGCAllocation)
    {
        /// Allocate a store pointer of length `capacity`.
        pragma(inline) private void allocateStoreWithCapacity(size_t capacity, bool zero = false) @trusted nothrow
        {
            if (zero) { _ptr = cast(E*)GC.calloc(capacity, E.sizeof); }
            else      { _ptr = cast(E*)GC.malloc(capacity * E.sizeof); }
            _capacity = capacity;
        }
    }
    else
    {
        /// Allocate a store pointer of length `capacity`.
        pragma(inline) private void allocateStoreWithCapacity(size_t capacity, bool zero = false) @trusted nothrow @nogc
        {
            if (zero) { _ptr = cast(E*)calloc(capacity, E.sizeof); }
            else      { _ptr = cast(E*)malloc(capacity * E.sizeof); }
            _capacity = capacity;
        }
    }

    static if (assignment == Assignment.copy)
    {
        /// Copy construction.
        this(this) nothrow @trusted
        {
            auto rhs_storePtr = _ptr; // save store pointer
            allocateStoreWithCapacity(_length);     // allocate new store pointer
            foreach (const i; 0 .. _length)
            {
                _ptr[i] = rhs_storePtr[i]; // copy from old to new
            }
            static if (shouldAddGCRange!E)
            {
                gc_addRange(_ptr, _length * E.sizeof);
            }
        }

        /// Copy assignment.
        void opAssign(typeof(this) rhs) @trusted
        {
            // self-assignment may happen when assigning derefenced pointer
            if (_ptr != rhs._ptr) // if not self assignment
            {
                reserve(rhs._length);
                foreach (const i; 0 .. _length)
                {
                    _ptr[i] = rhs._ptr[i]; // copy from old to new
                }
                static if (shouldAddGCRange!E)
                {
                    gc_addRange(_ptr, _length * E.sizeof);
                }
            }
        }
    }

    static if (assignment == Assignment.disabled ||
               assignment == Assignment.move)
    {
        @disable this(this);

        static if (isCopyable!E)
        {
            /// Returns: shallow duplicate of `this`.
            typeof(this) dup() nothrow @trusted const
            {
                typeof(return) copy;
                copy._length = this._length;
                copy.allocateStoreWithCapacity(this._length);     // allocate new store pointer
                foreach (const i; 0 .. _length)
                {
                    copy._ptr[i] = this._ptr[i]; // copy from old to new
                }
                static if (shouldAddGCRange!E)
                {
                    GC.addRange(copy._ptr, _length * E.sizeof);
                }
                return copy;
            }
        }
    }

    pragma(inline) void opAssign(typeof(null))
    {
        clear();
    }

    bool opEquals(const ref typeof(this) rhs) const @trusted
    {
        static if (isCopyable!E)
        {
            return this[] == rhs[]; // TODO fix DMD to make this work for non-copyable aswell
        }
        else
        {
            if (this._length != rhs._length) { return false; }
            foreach (const i; 0 .. _length)
            {
                if (this._ptr[i] != rhs._ptr[i]) { return false; }
            }
            return true;
        }
    }
    bool opEquals(in        typeof(this) rhs) const @trusted
    {
        static if (isCopyable!E)
        {
            return this[] == rhs[]; // TODO fix DMD to make this work for non-copyable aswell
        }
        else
        {
            if (this._length != rhs._length) { return false; }
            foreach (const i; 0 .. _length)
            {
                if (this._ptr[i] != rhs._ptr[i]) { return false; }
            }
            return true;
        }
    }

    /** Default-initialize all elements. */
    pragma(inline) void defaultInitialize() @trusted pure nothrow @nogc @("complexity", "O(length)")
    {
        memset(_ptr, 0, _length);
    }

    /** Construct from InputRange `values`.
        If `values` are sorted `assumeSortedParameter` is true.
     */
    this(R)(R values, bool assumeSortedParameter = false) @trusted pure nothrow @("complexity", "O(n*log(n))")
        if (isInputRange!R)
    {
        // init
        _ptr = null;
        _capacity = 0;

        // append new data
        import std.range.primitives : hasLength;
        static if (hasLength!R)
        {
            reserve(values.length); // fast reserve
            size_t i = 0;
            foreach (ref value; values)
            {
                ptr[i++] = value;
            }
            _length = values.length;
        }
        else
        {
            size_t i = 0;
            foreach (ref value; values)
            {
                reserve(i + 1); // slower reserve
                ptr[i++] = value;
            }
            _length = i;
        }

        static if (IsOrdered!ordering)
        {
            if (!assumeSortedParameter)
            {
                import std.algorithm.sorting : sort;
                sort!comp(ptr[0 .. _length]);
            }
        }
    }

    /// Reserve room for `n` elements at store `_ptr`.
    static if (useGCAllocation)
    {
        void reserve(size_t n) pure nothrow @trusted
        {
            makeReservedLengthAtLeast(n);
            static if (shouldAddGCRange!E) { gc_removeRange(_ptr); } // TODO move somewhere else?
            _ptr = cast(E*)GC.realloc(_ptr, E.sizeof * _capacity);
            static if (shouldAddGCRange!E) { gc_addRange(_ptr, _length * E.sizeof); } // TODO move somewhere else?
        }
    }
    else
    {
        void reserve(size_t n) pure nothrow @trusted @nogc
        {
            makeReservedLengthAtLeast(n);
            static if (shouldAddGCRange!E) { gc_removeRange(_ptr); } // TODO move somewhere else?
            _ptr = cast(E*)realloc(_ptr, E.sizeof * _capacity);
            static if (shouldAddGCRange!E) { gc_addRange(_ptr, _length * E.sizeof); } // TODO move somewhere else?
        }
    }

    /// Helper for `reserve`.
    private void makeReservedLengthAtLeast(size_t n) pure nothrow @safe @nogc
    {
        import std.math : nextPow2;
        if (_capacity < n) { _capacity = n.nextPow2; }
    }

    /// Pack/Compress storage.
    static if (useGCAllocation)
    {
        void compress() pure nothrow @trusted
        {
            if (_length)
            {
                _ptr = cast(E*)GC.realloc(_ptr, E.sizeof * _length);
                // TODO gc_removeRange with what arguments?
            }
            else
            {
                static if (shouldAddGCRange!E)
                {
                    gc_removeRange(_ptr);
                }
                GC.free(_ptr);
                _ptr = null;
            }
            _capacity = _length;
        }
    }
    else
    {
        void compress() pure nothrow @trusted @nogc
        {
            if (_length)
            {
                _ptr = cast(E*)realloc(_ptr, E.sizeof * _capacity);
            }
            else
            {
                free(_ptr);
                _ptr = null;
            }
            _capacity = _length;
        }
    }
    alias pack = compress;

    /// Destruct.
    static if (useGCAllocation)
    {
        nothrow @trusted:

        ~this() { release(); }

        void clear()
        {
            release();
            resetInternalData();
        }

        private void release()
        {
            destroyElements();
            static if (shouldAddGCRange!E)
            {
                gc_removeRange(_ptr);
            }
            GC.free(_ptr);
        }
    }
    else
    {
        nothrow @trusted @nogc:

        ~this() { release(); }

        void clear()
        {
            release();
            resetInternalData();
        }

        private void release()
        {
            destroyElements();
            static if (shouldAddGCRange!E)
            {
                gc_removeRange(_ptr);
            }
            free(_ptr);
        }
    }

    private void destroyElements()
    {
        import std.traits : hasElaborateDestructor;
        static if (hasElaborateDestructor!E)
        {
            foreach (const i; 0 .. _length)
            {
                .destroy(_ptr[i]);
            }
        }
    }

    private void resetInternalData()
    {
        _ptr = null;
        _length = 0;
        _capacity = 0;
    }

    enum isElementAssignable(U) = isAssignable!(E, U);

    /** Removal doesn't need to care about ordering. */
    ContainerElementType!(typeof(this), E) linearPopAtIndex(size_t index) @trusted @("complexity", "O(length)")
    {
        assert(index < _length);
        auto value = move(ptr[index]);
        // TODO use this instead:
        // const si = index + 1;   // source index
        // const ti = index;       // target index
        // const restLength = _length - (index + 1);
        // import std.algorithm.mutation : moveEmplaceAll;
        // moveEmplaceAll(ptr[si .. si + restLength],
        //                ptr[ti .. ti + restLength]);
        foreach (const i; 0 .. _length - (index + 1)) // each element index that needs to be moved
        {
            const si = index + i + 1; // source index
            const ti = index + i; // target index
            moveEmplace(ptr[si], ptr[ti]);
        }
        --_length;
        return value;
    }
    alias linearRemoveAt = linearPopAtIndex;
    alias linearDeleteAt = linearPopAtIndex;

    /** Removal doesn't need to care about ordering. */
    pragma(inline) ContainerElementType!(typeof(this), E) linearPopFront() @trusted @("complexity", "O(length)")
    {
        return linearPopAtIndex(0);
    }

    /** Removal doesn't need to care about ordering. */
    pragma(inline) void popBack() @safe @("complexity", "O(1)")
    {
        assert(!empty);
        --_length;
    }

    /** Pop back element and return it. */
    pragma(inline) E backPop() @trusted
    {
        assert(!empty);
        return move(ptr[--_length]); // TODO optimize by not clearing `ptr[--_length]` after move
        // TODO gc_removeRange
    }

    /** Pop last `count` back elements. */
    pragma(inline) void popBackN(size_t count) @safe @("complexity", "O(1)")
    {
        shrinkTo(_length - count);
    }

    static if (!IsOrdered!ordering) // for unsorted arrays
    {
        /// Push back (append) `values`.
        pragma(inline) void pushBack(Us...)(ref Us values) @("complexity", "O(1)")
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            pushBackHelper(values);
        }
        /// ditto
        void pushBack(R)(R values) @("complexity", "O(values.length)")
            if (isInputRange!R &&
                !(isArray!R) &&
                !(isMyArray!R) &&
                isElementAssignable!(ElementType!R))
        {
            // import std.range.primitives : hasLength;
            // static if (hasLength!R) { dln("Reuse logic in range constructor"); }
            foreach (ref value; values)
            {
                pushBackHelper(value);
            }
        }
        /// ditto.
        void pushBack(A)(A values) @trusted @("complexity", "O(values.length)")
            if (isArray!A &&
                isElementAssignable!(ElementType!A))
        {
            if (_ptr == values.ptr) // called as: this ~= this. TODO extend to check if `values` overlaps ptr[0 .. _capacity]
            {
                reserve(2*_length);
                foreach (const i; 0 .. _length)
                {
                    _ptr[_length + i] = _ptr[i]; // needs copying
                }
                _length *= 2;
            }
            else
            {
                reserve(_length + values.length);
                if (is(Unqual!E == Unqual!(ElementType!A)))
                {
                    // TODO reuse memcopy if ElementType!A is same as E)
                }
                foreach (const i, ref value; values)
                {
                    _ptr[_length + i] = value;
                }
                _length += values.length;
            }
        }
        /// ditto.
        void pushBack(A)(const ref A values) @trusted @("complexity", "O(values.length)") // TODO `in` parameter qualifier doesn't work here. Compiler bug?
            if (isMyArray!A &&
                isElementAssignable!(ElementType!A))
        {
            if (_ptr == values._ptr) // called as: this ~= this
            {
                reserve(2*_length);
                // NOTE: this is not needed because we don't need range checking here?:
                // _ptr[length .. 2*length] = values._ptr[0 .. length];
                foreach (const i; 0 .. _length)
                {
                    _ptr[_length + i] = values._ptr[i];
                }
                _length *= 2;
            }
            else
            {
                reserve(_length + values.length);
                if (is(Unqual!E == Unqual!(ElementType!A)))
                {
                    // TODO reuse memcopy if ElementType!A is same as E)
                }
                foreach (const i, ref value; values.slice)
                {
                    _ptr[_length + i] = value;
                }
                _length += values.length;
            }
        }
        alias append = pushBack;
        alias put = pushBack;

        // NOTE these separate overloads of opOpAssign are needed because one
        // `const ref`-parameter-overload doesn't work because of compiler bug
        // with: `this(this) @disable`
        pragma(inline) void opOpAssign(string op, Us...)(Us values)
            if (op == "~" &&
                values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            pushBack(values);
        }
	pragma(inline) void opOpAssign(string op, R)(R values)
            if (op == "~" &&
                isInputRange!R &&
                allSatisfy!(isElementAssignable, ElementType!R))
        {
            pushBack(values);
        }
	pragma(inline) void opOpAssign(string op, A)(const ref A values)
            if (op == "~" &&
                isMyArray!A &&
                isElementAssignable!(ElementType!A))
        {
            pushBack(values);
        }
    }

    static if (IsOrdered!ordering)
    {
        import std.range : SearchPolicy, assumeSorted;

        /// Returns: `true` iff this contains `value`.
        pragma(inline) bool contains(U)(U value) const nothrow @nogc @("complexity", "O(log(length))")
        {
            return this[].contains(value);
        }

        /** Wrapper for `std.range.SortedRange.lowerBound` when this `ordering` is sorted. */
        pragma(inline) auto lowerBound(SearchPolicy sp = SearchPolicy.binarySearch, U)(U e) inout @("complexity", "O(log(length))")
        {
            return this[].lowerBound!sp(e);
        }

        /** Wrapper for `std.range.SortedRange.upperBound` when this `ordering` is sorted. */
        pragma(inline) auto upperBound(SearchPolicy sp = SearchPolicy.binarySearch, U)(U e) inout @("complexity", "O(log(length))")
        {
            return this[].upperBound!sp(e);
        }

        static if (ordering == Ordering.sortedUniqueSet)
        {
            /** Inserts `values` into `this` ordered set.
                Returns: `bool`-array with same length as `values`, where i:th
                `bool` value is set if `value[i]` wasn't previously in `this`.
            */
            bool[Us.length] linearInsert(SearchPolicy sp = SearchPolicy.binarySearch, Us...)(Us values) @("complexity", "O(length)")
                if (values.length >= 1 &&
                    allSatisfy!(isElementAssignable, Us))
            in
            {
                // assert no duplicates in `values`
                import std.range : empty;
                import std.algorithm.searching : findAdjacent;
                import std.algorithm.sorting : sort;

                // TODO functionize or use other interface in pushing `values`
                import std.traits : CommonType;
                CommonType!Us[Us.length] valuesArray;
                foreach (const i, const ref value; values)
                {
                    valuesArray[i] = value;
                }
                assert(sort(valuesArray[]).findAdjacent.empty, "Parameter `values` must not contain duplicate elements");
            }
            body
            {
                static if (values.length == 1) // faster because `contains()` followed by `completeSort()` searches array twice
                {
                    static if (false)
                    {
                        import std.traits : CommonType;
                        size_t[Us.length] ixs;
                        CommonType!Us[Us.length] vs;
                        size_t i = 0;
                        foreach (const ref value; sort([values]))
                        {
                            const index = indexOf(value);
                            if (index != size_t.max)
                            {
                                ixs[i] = index;
                                vs[i] = value;
                                ++i;
                            }
                        }
                        // TODO insert them in one go in reverse starting from
                        // the end of this array
                    }

                    import searching_ex : containsStoreIndex;
                    size_t index;
                    if (slice.assumeSorted!comp.containsStoreIndex!sp(values, index)) // faster than `completeSort` for single value
                    {
                        return [false];
                    }
                    else
                    {
                        linearInsertAtIndexHelper(index, values);
                        return [true];
                    }
                }
                else
                {
                    import std.algorithm.sorting : completeSort;
                    debug { typeof(return) hits; }
                    else  { typeof(return) hits = void; }
                    size_t expandedLength = 0;
                    const initialLength = _length;
                    foreach (const i, ref value; values)
                    {
                        // TODO reuse completeSort with uniqueness handling?
                        static if (values.length == 1)
                        {
                            // TODO reuse single parameter overload linearUniqueInsert() and return
                        }
                        else
                        {
                            // TODO reuse completeSort with uniqueness handling?
                        }
                        hits[i] = !this[0 .. initialLength].contains(value);
                        if (hits[i])
                        {
                            pushBackHelper(value); // NOTE: append but don't yet sort
                            ++expandedLength;
                        }
                    }

                    if (expandedLength != 0)
                    {
                        const ix = _length - expandedLength;
                        completeSort!comp(_ptr[0 .. ix].assumeSorted!comp,
                                          _ptr[ix .. _length]);
                    }
                    return hits;
                }
            }
        }
        else static if (ordering == Ordering.sortedValues)
        {
            /** Inserts `values`. */
            void linearInsert(SearchPolicy sp = SearchPolicy.binarySearch, Us...)(Us values) @("complexity", "O(log(length))")
                if (values.length >= 1 &&
                    allSatisfy!(isElementAssignable, Us))
            {
                // TODO add optimization for values.length == 2
                static if (values.length == 1)
                {
                    import searching_ex : containsStoreIndex;
                    size_t index;
                    if (!slice.assumeSorted!comp.containsStoreIndex!sp(values, index)) // faster than `completeSort` for single value
                    {
                        linearInsertAtIndexHelper(index, values);
                    }
                }
                else
                {
                    import std.algorithm.sorting : completeSort;
                    pushBackHelper(values); // simpler because duplicates are allowed
                    const ix = _length - values.length;
                    completeSort!comp(_ptr[0 .. ix].assumeSorted!comp,
                                      _ptr[ix .. _length]);
                }
            }
        }
        alias linsert = linearInsert;
    }
    else
    {
        /** Insert element(s) `values` at array offset `index`. */
        pragma(inline) void linearInsertAtIndex(Us...)(size_t index, Us values) nothrow @("complexity", "O(length)")
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            linearInsertAtIndexHelper(index, values);
        }

        /** Insert element(s) `values` at the beginning. */
        pragma(inline) void linearPushFront(Us...)(Us values) nothrow @("complexity", "O(length)")
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            linearInsertAtIndex(0, values);
        }

        alias prepend = linearPushFront;
    }

    /** Helper function used externally for unsorted and internally for sorted. */
    private void linearInsertAtIndexHelper(Us...)(size_t index, Us values) nothrow @("complexity", "O(length)")
    {
        reserve(_length + values.length);

        // TODO factor this to robustCopy. It uses copy when no overlaps (my algorithm_em), iteration otherwise
        enum usePhobosCopy = false;
        static if (usePhobosCopy)
        {
            // TODO why does this fail?
            import std.algorithm.mutation : copy;
            copy(_ptr[index ..
                     _length],        // source
                 _ptr[index + values.length ..
                     _length + values.length]); // target
        }
        else
        {
            // move second part in reverse
            // TODO functionize move
            foreach (const i; 0 .. _length - index) // each element index that needs to be moved
            {
                const si = _length - 1 - i; // source index
                const ti = si + values.length; // target index
                _ptr[ti] = _ptr[si]; // TODO move construct?
            }
        }

        // set new values
        foreach (const i, ref value; values)
        {
            _ptr[index + i] = value; // TODO use range algorithm instead?
        }

        _length += values.length;
    }

    private void pushBackHelper(Us...)(ref Us values) @trusted nothrow @("complexity", "O(1)")
    {
        reserve(_length + values.length);
        foreach (const i, ref value; values)
        {
            moveEmplace(value, _ptr[_length + i]);
        }
        _length += values.length;
    }

    @property @("complexity", "O(1)")
    pragma(inline, true):

    /// ditto
    static if (IsOrdered!ordering)
    {
    const nothrow @nogc:                      // indexing and slicing must be `const` when ordered

        /// Slice operator must be const when ordered.
        auto opSlice()          // unsafe!
        {
            return opSlice!(typeof(this))(0, _length);
        }
        /// ditto
        auto opSlice(this This)(size_t i, size_t j) // const because mutation only via `op.*Assign`
        {
            // alias ET = ContainerElementType!(This, E);
            import std.range : assumeSorted;
            return (cast(const(E)[])slice[i .. j]).assumeSorted!comp;
        }

        auto ref opIndex(size_t i) @trusted
        {
            // alias ET = ContainerElementType!(typeof(this), E);
            assert(i < _length);
            return _ptr[i];
        }

        /// Get front element (as constant reference to preserve ordering).
        ref const(E) front() @trusted
        {
            assert(!empty);
            return _ptr[0];
        }

        /// Get back element (as constant reference to preserve ordering).
        ref const(E) back() @trusted
        {
            assert(!empty);
            return _ptr[_length - 1];
        }
    }
    else
    {
        nothrow:

        @property void length(size_t newLength) @safe
        {
            reserve(newLength);
            _length = newLength;
        }

        inout:               // indexing and slicing can be mutable when ordered

        /// Slice operator overload is mutable when unordered.
        auto opSlice()          // unsafe!
        {
            return this.opSlice(0, _length);
        }
        /// ditto
        auto opSlice(size_t i, size_t j) // unsafe!
        {
            // alias ET = ContainerElementType!(typeof(this), E);
            assert(i <= j);
            assert(j <= _length);
            return _ptr[i .. j];
        }

        /// Index operator can be const or mutable when unordered.
        auto ref opIndex(size_t i) @trusted
        {
            // alias ET = ContainerElementType!(typeof(this), E);
            assert(i < _length);
            return _ptr[i];
        }

        /// Get front element reference.
        ref inout(E) front() inout @trusted
        {
            assert(!empty);
            return _ptr[0];
        }

        /// Get back element reference.
        ref inout(E) back() inout @trusted
        {
            assert(!empty);
            return _ptr[_length - 1];
        }
    }

    pure nothrow:

    @nogc:

    /// Check if empty.
    bool empty() const @safe
    {
        return _length == 0;
    }

    /// Get length.
    size_t length() const @safe
    {
        return _length;
    }
    alias opDollar = length;    /// ditto

    /// Shrink length to `length`.
    void shrinkTo(size_t newLength) @safe
    {
        assert(newLength <= _length);
        _length = newLength;
    }
    alias opDollar = length;    /// ditto

    /// Get length of reserved store.
    size_t reservedLength() const @safe
    {
        return _capacity;
    }
    alias capacity = reservedLength;

    /// Get internal pointer.
    private inout(E*) ptr() inout
    {
        // TODO Use cast(ET[])?: alias ET = ContainerElementType!(typeof(this), E);
        return _ptr;
    }

    /// Get internal slice.
    private auto ref slice() inout @trusted
    {
        return _ptr[0 .. _length];
    }

private:
    // TODO reuse module `storage` for small size/array optimization (SSO)
    E* _ptr;               // store pointer
    size_t _capacity;      // store capacity
    size_t _length;             // length
}

alias SortedArray(E, Assignment assignment = Assignment.disabled,
                  bool useGCAllocation = false,
                  alias less = "a < b") = Array!(E, assignment, Ordering.sortedValues, useGCAllocation, less);

alias SortedSetArray(E, Assignment assignment = Assignment.disabled,
                     bool useGCAllocation = false,
                     alias less = "a < b") = Array!(E, assignment, Ordering.sortedUniqueSet, useGCAllocation, less);

unittest
{
    import std.conv : to;
    foreach (assignment; AliasSeq!(Assignment.disabled, Assignment.copy))
    {
        foreach (Ch; AliasSeq!(char, wchar, dchar))
        {
            alias Str = Array!(Ch, assignment);
            Str str_as = Str.withElement('a');
            str_as ~= '_'.to!Ch;
            assert(str_as[].equal("a_"));
        }
    }
}

static void tester(Ordering ordering, bool supportGC, alias less)()
{
    import std.functional : binaryFun;
    import std.range : iota, retro, chain, repeat, only, ElementType;
    import std.algorithm : filter, map;
    import std.algorithm.sorting : isSorted, sort;
    import std.exception : assertThrown, assertNotThrown;
    import std.traits : isInstanceOf;
    import std.typecons : Unqual;

    enum assignment = Assignment.copy;
    alias comp = binaryFun!less; //< comparison

    alias E = int;

    foreach (Ch; AliasSeq!(char, wchar, dchar))
    {
        alias Str = Array!(Ch, assignment, ordering, supportGC, less);
        Str str;
        static assert(is(Unqual!(ElementType!Str) == Ch));
        static assert(str.isString);
        str = Str.init;         // inhibit Dscanner warning
    }

    static if (E.sizeof == 4)
    {
        foreach (const n; [0, 1, 2, 3, 4])
        {
            assert(Array!(E, assignment, ordering, supportGC, less).withLength(n).isSmall);
        }
        assert(!(Array!(E, assignment, ordering, supportGC, less).withLength(5).isSmall));
    }

    // test move construction
    {
        const maxLength = 1024;
        foreach (const n; 0 .. maxLength)
        {
            auto x = Array!(E, assignment, ordering, supportGC, less).withLength(n);

            // test resize
            static if (!IsOrdered!ordering)
            {
                assert(x.length == n);
                x.length = n + 1;
                assert(x.length == n + 1);
                x.length = n;
            }

            const ptr = x.ptr;
            const capacity = x.capacity;
            assert(x.length == n);

            import std.algorithm.mutation : move;
            auto y = Array!(E, assignment, ordering, supportGC, less)();
            move(x, y);

            assert(x.length == 0);
            assert(x.capacity == 0);
            assert(x.ptr == null);

            assert(y.length == n);
            assert(y.capacity == capacity);
            assert(y.ptr == ptr);

        }
    }

    foreach (const n; chain(0.only,
                            iota(0, 10).map!(x => 2^^x)))
    {
        import std.array : array;
        import std.range : radial, retro;

        const zi = cast(int)0;
        const ni = cast(int)n;

        auto fw = iota(zi, ni); // 0, 1, 2, ..., n-1

        // TODO use radial instead
        auto bw = fw.array.radial;

        Array!(E, assignment, ordering, supportGC, less) ss0 = bw; // reversed
        static assert(is(Unqual!(ElementType!(typeof(ss0))) == E));
        static assert(isInstanceOf!(Array, typeof(ss0)));
        assert(ss0.length == n);

        static if (IsOrdered!ordering)
        {
            if (!ss0.empty) { assert(ss0[0] == ss0[0]); } // trigger use of opindex
            assert(ss0[].equal(fw.array.sort!comp));
            assert(ss0[].isSorted!comp);
        }

        Array!(E, assignment, ordering, supportGC, less) ss1 = fw; // ordinary
        assert(ss1.length == n);

        static if (IsOrdered!ordering)
        {
            assert(ss1[].equal(fw.array.sort!comp));
            assert(ss1[].isSorted!comp);
        }

        Array!(E, assignment, ordering, supportGC, less) ss2 = fw.filter!(x => x & 1);
        assert(ss2.length == n/2);

        static if (IsOrdered!ordering)
        {
            assert(ss2[].equal(fw.filter!(x => x & 1).array.sort!comp));
            assert(ss2[].isSorted!comp);
        }

        auto ssA = Array!(E, assignment, ordering, supportGC, less).withLength(0);
        static if (IsOrdered!ordering)
        {
            static if (less == "a < b")
            {
                alias A = Array!(E, assignment, ordering, supportGC, less);
                const A x = [1, 2, 3, 4, 5, 6];
                assert(x.front == 1);
                assert(x.back == 6);
                assert(x.lowerBound(3).equal([1, 2]));
                assert(x.upperBound(3).equal([4, 5, 6]));
            }

            foreach (i; bw)
            {
                static if (ordering == Ordering.sortedUniqueSet)
                {
                    assert(ssA.linearInsert(i)[].equal([true]));
                    assert(ssA.linearInsert(i)[].equal([false]));
                }
                else
                {
                    ssA.linearInsert(i);
                }
            }
            assert(ssA[].equal(sort!comp(fw.array)));

            auto ssB = Array!(E, assignment, ordering, supportGC, less).withLength(0);
            static if (ordering == Ordering.sortedUniqueSet)
            {
                assert(ssB.linearInsert(1, 7, 4, 9)[].equal(true.repeat(4)));
                assert(ssB.linearInsert(3, 6, 8, 5, 1, 9)[].equal([true, true, true, true, false, false]));
                assert(ssB.linearInsert(3, 0, 2, 10, 11, 5)[].equal([false, true, true, true, true, false]));
                assert(ssB.linearInsert(0, 2, 10, 11)[].equal(false.repeat(4))); // false becuse already inserted
                assert(ssB.reservedLength == 16);
            }
            else
            {
                ssB.linearInsert(1, 7, 4, 9);
                ssB.linearInsert(3, 6, 8, 5);
                ssB.linearInsert(0, 2, 10, 11);
                assert(ssB.reservedLength == 16);
            }

            auto ssI = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].sort!comp; // values
            const ssO = [12, 13]; // values not range

            assert(ssB[].equal(ssI));

            foreach (s; ssI) { assert(ssB.contains(s)); }
            foreach (s; ssO) { assert(!ssB.contains(s)); }

            ssB.compress;
            assert(ssB.reservedLength == 12);
        }
        else
        {
            {
                alias A = Array!(E, assignment, ordering, supportGC);
                A x = [1, 2, 3];
                x ~= x;
                assert(x[].equal([1, 2, 3,
                                  1, 2, 3]));
                x ~= x[];
                assert(x[].equal([1, 2, 3, 1, 2, 3,
                                  1, 2, 3, 1, 2, 3]));
            }

            ssA ~= 3;
            ssA ~= 2;
            ssA ~= 1;
            assert(ssA[].equal([3, 2, 1]));
            assert(ssA.reservedLength == 4);

            ssA.compress;
            assert(ssA.reservedLength == 3);

            // popBack
            ssA[0] = 1;
            ssA[1] = 2;
            assert(ssA[].equal([1, 2, 1]));
            assert(!ssA.empty);
            assert(ssA.front == 1);
            assert(ssA.back == 1);

            assertNotThrown(ssA.popBack);
            assert(ssA[].equal([1, 2]));
            assert(!ssA.empty);
            assert(ssA.front == 1);
            assert(ssA.back == 2);

            assertNotThrown(ssA.popBack);
            assert(ssA[].equal([1]));
            assert(!ssA.empty);
            assert(ssA.front == 1);
            assert(ssA.back == 1);

            assertNotThrown(ssA.popBack);
            assert(ssA.length == 0);
            assert(ssA.empty);
            assert(ssA.reservedLength != 0);

            ssA.compress;
            assert(ssA.length == 0);
            assert(ssA.reservedLength == 0);
            assert(ssA.empty);

            // linearInsertAt
            ssA ~= 1;
            ssA ~= 2;
            ssA ~= 3;
            ssA ~= 4;
            ssA ~= 5;
            ssA ~= 6;
            ssA ~= 7;
            ssA ~= 8;
            assert(ssA[].equal([1, 2, 3, 4, 5, 6, 7, 8]));
            ssA.linearInsertAtIndex(3, 100, 101);
            assert(ssA[].equal([1, 2, 3, 100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront);
            assert(ssA[].equal([2, 3, 100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront);
            assert(ssA[].equal([3, 100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront);
            assert(ssA[].equal([100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront);
            assertNotThrown(ssA.linearPopFront);
            assertNotThrown(ssA.linearPopFront);
            assertNotThrown(ssA.linearPopFront);
            assertNotThrown(ssA.linearPopFront);
            assertNotThrown(ssA.linearPopFront);
            assertNotThrown(ssA.linearPopFront);
            assert(ssA.empty);
            ssA.compress;

            // linearPopAtIndex
            ssA ~= 1;
            ssA ~= 2;
            ssA ~= 3;
            ssA ~= 4;
            ssA ~= 5;
            assertNotThrown(ssA.linearPopAtIndex(2));
            assert(ssA[].equal([1, 2, 4, 5]));

            // pushBack and assignment from slice
            auto ssB = Array!(E, assignment, ordering, supportGC, less).withLength(0);
            ssB.pushBack([1, 2, 3, 4, 5]);
            ssB.pushBack([6, 7]);
            assert(ssB[].equal([1, 2, 3, 4, 5, 6, 7]));
            assert(ssB.backPop == 7);
            assert(ssB.backPop == 6);
            assert(ssB.backPop == 5);
            assert(ssB.backPop == 4);
            assert(ssB.backPop == 3);
            assert(ssB.backPop == 2);
            assert(ssB.backPop == 1);
            assert(ssB.empty);

            // pushBack(Array)
            {
                const s = [1, 2, 3];
                Array!(E, assignment, ordering, supportGC, less) s1 = s;
                Array!(E, assignment, ordering, supportGC, less) s2 = s1[];
                assert(s1[].equal(s));
                s1 ~= s1;
                assert(s1[].equal(chain(s, s)));
                s1 ~= s2;
                assert(s1[].equal(chain(s, s, s)));
            }

            const ss_ = Array!(E, assignment, ordering, supportGC, less)(null);
            assert(ss_.empty);

            auto ssC = Array!(E, assignment, ordering, supportGC, less).withLength(0);
            const(int)[] i5 = [1, 2, 3, 4, 5];
            ssC.pushBack(i5);
            assert(ssC[].equal(i5));

            auto ssCc = ssC;    // copy it
            assert(ssCc[].equal(i5));

            ssC.shrinkTo(4);
            assert(ssC[].equal([1, 2, 3, 4]));

            ssC.shrinkTo(3);
            assert(ssC[].equal([1, 2, 3]));

            ssC.shrinkTo(2);
            assert(ssC[].equal([1, 2]));

            ssC.shrinkTo(1);
            assert(ssC[].equal([1]));

            ssC.shrinkTo(0);
            assert(ssC[].length == 0);
            assert(ssC.empty);

            ssC.pushBack(i5);
            ssC.popBackN(3);
            assert(ssC[].equal([1, 2]));

            auto ssD = ssC;
            ssC.clear();
            assert(ssC.empty);

            assert(!ssD.empty);
            ssD = null;
            assert(ssD.empty);
            assert(ssD == typeof(ssD).init);

            assert(ssCc[].equal(i5));

            ssCc = ssCc;   // self assignment
        }
    }
}

/// disabled copying
@safe nothrow unittest
{
    import std.conv : to;
    alias E = string;
    alias A = Array!(E, Assignment.disabled, Ordering.unsorted, false, "a < b");
    A a;
    const n = 100_000;
    size_t i = 0;
    foreach (const ref e; 0 .. n)
    {
        a ~= e.to!E;
        assert(a.length == i + 1);
        ++i;
    }
    const b = a.dup;
    assert(b.length == a.length);
    assert(a !is b);
    assert(a == b);
}

/// disabled copying
nothrow unittest
{
    import std.conv : to;
    alias E = string;
    alias A = Array!(E, Assignment.disabled, Ordering.unsorted, false, "a < b");
    A a;
    const n = 100_000;
    size_t i = 0;
    foreach (const ref e; 0 .. n)
    {
        a ~= e.to!E;
        assert(a.length == i + 1);
        ++i;
    }
    const b = a.dup;
    assert(a[] == b[]);
}

/// disabled copying
nothrow unittest
{
    alias E = string;
    alias A = Array!E;
    import std.traits : isCopyable, isRvalueAssignable, isLvalueAssignable;
    static assert(!isCopyable!(A));
    static assert(isRvalueAssignable!(A));
    static assert(isLvalueAssignable!(A));
    alias AA = Array!A;
    AA aa;
    A a;
    a ~= "string";
    aa ~= A.init;

    assert(aa == aa);
    assert(AA.withLength(3) == AA.withLength(3));
    assert(AA.withCapacity(3) == AA.withCapacity(3));
    assert(AA.withLength(3).length == 3);
    assert(aa != AA.init);
}

///
nothrow @nogc unittest
{
    alias E = int;
    alias A = Array!E;
    A a;
    import std.range : iota;
    import std.container.util : make;
    foreach (n; 0 .. 100)
    {
        const e = iota(0, n).make!Array;
        assert(e[].equal(iota(0, n)));
    }
}

version(unittest)
{
    import std.traits : EnumMembers;
}

/// use GC
pure nothrow unittest
{
    foreach (ordering; EnumMembers!Ordering)
    {
        tester!(ordering, true, "a < b"); // use GC
        tester!(ordering, true, "a > b"); // use GC
    }
}

/// don't use GC
pure nothrow /+TODO @nogc+/ unittest
{
    foreach (ordering; EnumMembers!Ordering)
    {
        tester!(ordering, false, "a < b"); // don't use GC
        tester!(ordering, false, "a > b"); // don't use GC
    }
}

pure nothrow unittest
{
    alias E = int;
    alias A = Array!E;
    A[string] map;
    map["a"] = A.init;
    map["B"] = A.withLength(42);

    auto aPtr = "a" in map;
    assert(aPtr);
    assert(A.init == *aPtr);
    assert(*aPtr == A.init);

    assert("z" !in map);
    auto zPtr = "z" in map;
    assert(!zPtr);
}

/// test withElement and withElements
@safe pure nothrow @nogc unittest
{
    import std.algorithm.mutation : move;
    import std.range : ElementType;

    alias A = Array!int;
    alias AA = Array!A;
    alias AAA = Array!AA;

    foreach (A_; AliasSeq!(A, AA, AAA))
    {
        alias E = ElementType!A_;
        A_ x = A_.withElement(E.init);
        A_ y = A_.withElements(E.init, E.init);
        assert(x.length == 1);
        assert(y.length == 2);
        foreach (_; 0 .. 1000)
        {
            auto e = E.init;
            x ~= move(e);
            y ~= E.init;
        }
        foreach (_; 0 .. 1000)
        {
            assert(x.backPop == E.init);
            assert(y.backPop == E.init);
        }
        assert(x.length == 1);
        assert(y.length == 2);
    }

}
