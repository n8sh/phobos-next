/** Array container(s) with optional sortedness (`Ordering`).

    TODO Support scope in auto ref returns when it DIP-1000 is ready

    TODO Should checkEmptyPop() only be enabled in debug?

    TODO Use std.array.insertInPlace in insert()?
    TODO Use std.array.replaceInPlace?

    TODO Split up `Array` into `Array`, `SortedArray`, `SetArray` and reuse
    logic in `Array` via `alias this` or free functions.

    TODO Use `std.algorithm.mutation.move` and `std.range.primitives.moveAt`

    TODO copy assignment, struct Store, Notify andralex of packed array

    TODO Add `c.insertAfter(r, x)` where `c` is a collection, `r` is a range
    previously extracted from `c`, and `x` is a value convertible to
    collection's element type. See also:
    https://forum.dlang.org/post/n3qq6e$2bis$1@digitalmars.com
 */
module array_ex;

// private import std.experimental.allocator.mallocator : Mallocator;

// we handle these as pure to make containers using them pure
extern(C) pure nothrow @system @nogc
{
    void* malloc(size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}

enum Ordering
{
    unsorted, // unsorted array
    sortedValues, // sorted array with possibly duplicate values
    sortedUniqueSet, // sorted array with unique values
}

version(unittest)
{
    import std.algorithm.comparison : equal;
    import std.meta : AliasSeq;
}

enum IsOrdered(Ordering ordering) = ordering != Ordering.unsorted;

import std.math : nextPow2;

template shouldAddGCRange(T)
{
    import std.traits : isPointer, hasIndirections;
    enum shouldAddGCRange = isPointer!T || hasIndirections!T || is (T == class);
}

/// Large array storage.
static struct Large(E, bool useGC)
{
    E* ptr;
    size_t length;

    static if (useGC)
    {
        import core.memory : GC;
    }
    else
    {
        alias _malloc = malloc;
        alias _realloc = realloc;
        alias _free = free;
    }

    pure nothrow:

    static if (useGC)
    {
        this(size_t n)
        {
            length = n;
            ptr = cast(E*)GC.malloc(E.sizeof * length);
        }
        void resize(size_t n)
        {
            length = n;
            ptr = cast(E*)GC.realloc(ptr, E.sizeof * length);
        }
        void clear()
        {
            GC.free(ptr); debug ptr = null;
        }
    }
    else
    {
        @nogc:
        this(size_t n)
        {
            length = n;
            ptr = cast(E*)_malloc(E.sizeof * length);
        }
        void resize(size_t n)
        {
            length = n;
            ptr = cast(E*)_realloc(ptr, E.sizeof * length);
        }
        void clear()
        {
            _free(ptr); debug ptr = null;
        }
    }
}

/// Small array storage.
alias Small(E, size_t n) = E[n];

/// Small-size-optimized (SSO) array store.
static struct Store(E, bool useGC = shouldAddGCRange!E)
{
    /** Fixed number elements that fit into small variant storage. */
    enum smallLength = Large!(E, useGC).sizeof / E.sizeof;

    /** Maximum number elements that fit into large variant storage. */
    enum maxLargeLength = size_t.max >> 8;

    /// Destruct.
    ~this() nothrow @trusted
    {
        if (isLarge) { large.clear; }
    }

    /// Get currently length at `ptr`.
    size_t length() const @trusted pure nothrow @nogc
    {
        return isLarge ? large.length : smallLength;
    }

    /// Returns: `true` iff is small packed.
    bool isSmall() const @safe pure nothrow @nogc { return !isLarge; }

private:

    /// Reserve length to `n` elements starting at `ptr`.
    void reserve(size_t n) pure nothrow @trusted
    {
        if (isLarge)        // currently large
        {
            if (n > smallLength) // large => large
            {
                large.resize(n);
            }
            else                // large => small
            {
                // large => tmp

                // temporary storage for small
                debug { typeof(small) tmp; }
                else  { typeof(return) tmp = void; }

                tmp[0 .. n] = large.ptr[0 .. n]; // large to temporary
                tmp[n .. $] = 0; // zero remaining

                // empty large
                large.clear();

                // tmp => small
                small[] = tmp[0 .. smallLength];

                isLarge = false;
            }
        }
        else                    // currently small
        {
            if (n > smallLength) // small => large
            {
                typeof(small) tmp = small; // temporary storage for small

                import std.conv : emplace;
                emplace(&large, n);

                large.ptr[0 .. length] = tmp[0 .. length]; // temporary to large

                isLarge = true;                      // tag as large
            }
            else {}                // small => small
        }
    }

    /// Get pointer.
    auto ptr() pure nothrow @nogc
    {
        alias ET = ContainerElementType!(typeof(this), E);
        return isLarge ? cast(ET*)large.ptr : cast(ET*)&small;
    }

    /// Get slice.
    auto ref slice() pure nothrow @nogc
    {
        return ptr[0 .. length];
    }

    union
    {
        Small!(E, smallLength) small; // small variant
        Large!(E, useGC) large;          // large variant
    }
    bool isLarge;               // TODO make part of union as in rcstring.d
}

/// Test `Store`.
static void storeTester(E, bool useGC)()
{
    Store!(E, useGC) si;

    assert(si.ptr !is null);
    assert(si.slice.ptr !is null);
    assert(si.slice.length != 0);
    assert(si.length == si.smallLength);

    si.reserve(si.smallLength);     // max small
    assert(si.length == si.smallLength);
    assert(si.isSmall);

    si.reserve(si.smallLength + 1); // small to large
    assert(si.length == si.smallLength + 1);
    assert(si.isLarge);

    si.reserve(si.smallLength * 8); // small to large
    assert(si.length == si.smallLength * 8);
    assert(si.isLarge);

    si.reserve(si.smallLength);     // max small
    assert(si.length == si.smallLength);
    assert(si.isSmall);

    si.reserve(0);
    assert(si.length == si.smallLength);
    assert(si.isSmall);

    si.reserve(si.smallLength + 1);
    assert(si.length == si.smallLength + 1);
    assert(si.isLarge);

    si.reserve(si.smallLength);
    assert(si.length == si.smallLength);
    assert(si.isSmall);

    si.reserve(si.smallLength - 1);
    assert(si.length == si.smallLength);
    assert(si.isSmall);
}


pure nothrow @nogc unittest
{
    foreach (E; AliasSeq!(char, byte, short, int))
    {
        storeTester!(E, false);
    }
}

pure nothrow unittest
{
    foreach (E; AliasSeq!(char, byte, short, int))
    {
        storeTester!(E, true);
    }
}

/// Returns: `true` iff C is an `Array`.
import std.traits : isInstanceOf;
enum isThisArray(C) = isInstanceOf!(Array, C);

/** Small-size-optimized (SSO-packed) array of value types `E` with optional
    ordering given by `ordering`.
 */
struct Array(E,
             Ordering ordering = Ordering.unsorted,
             bool useGC = shouldAddGCRange!E,
             alias less = "a < b") // TODO move out of this definition and support only for the case when `ordering` is not `Ordering.unsorted`
{
    import std.range : isInputRange, ElementType;
    import std.traits : isAssignable, Unqual, isSomeChar, isArray;
    import std.functional : binaryFun;
    import std.meta : allSatisfy;

    // import core.exception : RangeError;

    alias ME = Unqual!E; // mutable E
    enum isString = isSomeChar!E;

    alias comp = binaryFun!less; //< comparison

    /// Maximum number of elements that fits in SSO-packed
    enum smallLength = (_storeCapacity.sizeof + _length.sizeof) / E.sizeof;

    /// Returns: `true` iff is SSO-packed.
    bool isSmall() const @safe pure nothrow @nogc { return length <= smallLength; }

    static if (useGC)
    {
        import core.memory : GC;
    }
    else
    {
        alias _malloc = malloc;
        alias _realloc = realloc;
        alias _free = free;
    }

    /// Construct with length `n`.
    static if (useGC)
    {
        this(size_t n) @trusted nothrow
        {
            _storePtr = cast(E*)GC.malloc(E.sizeof * n);
            static if (shouldAddGCRange!E)
            {
                import core.memory : GC;
                GC.addRange(ptr, length * E.sizeof);
            }
            _length = _storeCapacity = n;
            defaultInitialize;
        }
    }
    else
    {
        this(size_t n) nothrow @trusted @nogc
        {
            _storePtr = cast(E*)_malloc(E.sizeof * n);
            static if (shouldAddGCRange!E)
            {
                import core.memory : GC;
                GC.addRange(ptr, length * E.sizeof);
            }
            _length = _storeCapacity = n;
            defaultInitialize;
        }
    }

    this(this) @disable;       /// TODO activate when internal RC-logic is ready

    /** Default-initialize all elements to `zeroValue`.. */
    void defaultInitialize(E zeroValue = E.init) @("complexity", "O(length)")
    {
        ptr[0 .. length] = zeroValue; // NOTE should we zero [0 .. _storeCapacity] instead?
    }

    /** Construct from InputRange `values`.
        If `values` are sorted `assumeSortedParameter` is true.
     */
    this(R)(R values, bool assumeSortedParameter = false) @trusted nothrow @("complexity", "O(n*log(n))")
        if (isInputRange!R)
    {
        // init
        _storePtr = null;
        _storeCapacity = 0;

        // append new data
        import std.range : hasLength;
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

    /// Reserve room for `n` elements at store `_storePtr`.
    static if (useGC)
    {
        void reserve(size_t n) pure nothrow @trusted
        {
            makeReservedLengthAtLeast(n);
            _storePtr = cast(E*)GC.realloc(_storePtr, E.sizeof * _storeCapacity);
            static if (shouldAddGCRange!E) { GC.addRange(ptr, length * E.sizeof); }
        }
    }
    else
    {
        void reserve(size_t n) pure nothrow @trusted @nogc
        {
            makeReservedLengthAtLeast(n);
            _storePtr = cast(E*)_realloc(_storePtr, E.sizeof * _storeCapacity);
        }
    }

    /// Helper for `reserve`.
    private void makeReservedLengthAtLeast(size_t n) pure nothrow @safe @nogc
    {
        if (_storeCapacity < n) { _storeCapacity = n.nextPow2; }
    }

    /// Pack/Compress storage.
    static if (useGC)
    {
        void compress() pure nothrow @trusted
        {
            if (length)
            {
                _storePtr = cast(E*)GC.realloc(_storePtr, E.sizeof * _length);
            }
            else
            {
                GC.free(_storePtr); debug _storePtr = null;
            }
            _storeCapacity = _length;
        }
    }
    else
    {
        void compress() pure nothrow @trusted @nogc
        {
            if (length)
            {
                _storePtr = cast(E*)_realloc(_storePtr, E.sizeof * _storeCapacity);
            }
            else
            {
                _free(_storePtr); debug _storePtr = null;
            }
            _storeCapacity = _length;
        }
    }
    alias pack = compress;

    /// Destruct.
    static if (useGC)
    {
        nothrow @trusted:

        ~this() { release(); }

        void clear()
        {
            release();
            reset();
        }

        private void release()
        {
            static if (shouldAddGCRange!E)
            {
                import core.memory : GC;
                GC.removeRange(ptr);
            }
            GC.free(_storePtr);
        }
    }
    else
    {
        nothrow @trusted @nogc:

        ~this() { release(); }

        void clear()
        {
            release();
            reset();
        }

        private void release()
        {
            static if (shouldAddGCRange!E)
            {
                import core.memory : GC;
                GC.removeRange(ptr);
            }
            _free(_storePtr);
        }
    }

    private void reset()
    {
        _storePtr = null;
        _length = 0;
        _storeCapacity = 0;
    }

    enum isElementAssignable(U) = isAssignable!(E, U);

    /** Removal doesn't need to care about ordering. */
    ContainerElementType!(typeof(this), E) linearPopAtIndex(size_t index) @trusted @("complexity", "O(length)")
    {
        assert(index < _length); // if (index >= _length) { throw new RangeError(); }
        checkEmptyPop();
        typeof(return) value = ptr[index]; // TODO move construct?
        // TODO functionize move
        foreach (const i; 0 .. length - (index + 1)) // each element index that needs to be moved
        {
            const si = index + i + 1; // source index
            const ti = index + i; // target index
            ptr[ti] = ptr[si]; // TODO move construct?
        }
        --_length;
        return value;
    }
    alias linearRemoveAt = linearPopAtIndex;
    alias linearDeleteAt = linearPopAtIndex;

    /** Removal doesn't need to care about ordering. */
    ContainerElementType!(typeof(this), E) linearPopFront() @trusted @("complexity", "O(length)")
    {
        checkEmptyPop();
        typeof(return) value = ptr[0]; // TODO move construct?
        // TODO functionize move
        foreach (const i; 0 .. length - 1) // each element index that needs to be moved
        {
            const si = i + 1; // source index
            const ti = i; // target index
            ptr[ti] = ptr[si]; // TODO move construct?
        }
        --_length;
        return value;
    }

    /** Removal doesn't need to care about ordering. */
    ContainerElementType!(typeof(this), E) popBack() @trusted @("complexity", "O(1)")
    {
        checkEmptyPop();
        return ptr[--_length]; // TODO move construct?
    }

    /** Pop back element and return it. */
    E backPop()
    {
        assert(!empty);
        E value = back;
        popBack();
        return value;
    }

    private void checkEmptyPop()
    {
        if (empty) { assert(false, `Cannot pop value from an empty array`); }
    }

    static if (!IsOrdered!ordering) // for unsorted arrays
    {
        /// Push back (append) `values`.
        void pushBack(Us...)(Us values) @("complexity", "O(1)")
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            pushBackHelper(values);
        }
        /// ditto
        void pushBack(R)(R values) @("complexity", "O(values.length)")
            if (isInputRange!R &&
                !(isArray!R) &&
                !(isThisArray!R) &&
                isElementAssignable!(ElementType!R))
        {
            // import std.range : hasLength;
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
            if (ptr == values.ptr) // called as: this ~= this
            {
                reserve(2*length);
                // NOTE: this is not needed because we don't need range checking here?:
                // ptr[length .. 2*length] = values.ptr[0 .. length];
                foreach (const i; 0 .. length) { ptr[length + i] = values.ptr[i]; } // TODO move. reuse memcpy
                _length *= 2;
            }
            else
            {
                reserve(length + values.length);
                if (is(Unqual!E == Unqual!(ElementType!A)))
                {
                    // TODO reuse memcopy if ElementType!A is same as E)
                }
                foreach (const i, ref value; values)
                {
                    ptr[length + i] = value;
                }
                _length += values.length;
            }
        }
        /// ditto.
        void pushBack(A)(const ref A values) @trusted @("complexity", "O(values.length)") // TODO `in` parameter qualifier doesn't work here. Compiler bug?
            if (isThisArray!A &&
                isElementAssignable!(ElementType!A))
        {
            if (ptr == values.ptr) // called as: this ~= this
            {
                reserve(2*length);
                // NOTE: this is not needed because we don't need range checking here?:
                // ptr[length .. 2*length] = values.ptr[0 .. length];
                foreach (const i; 0 .. length) { ptr[length + i] = values.ptr[i]; } // TODO move. reuse memcpy
                _length *= 2;
            }
            else
            {
                reserve(length + values.length);
                if (is(Unqual!E == Unqual!(ElementType!A)))
                {
                    // TODO reuse memcopy if ElementType!A is same as E)
                }
                foreach (const i, ref value; values.slice)
                {
                    ptr[length + i] = value;
                }
                _length += values.length;
            }
        }
        alias append = pushBack;

        // NOTE these separate overloads of opOpAssign are needed because one
        // `const ref`-parameter-overload doesn't work because of compiler bug
        // with: `this(this) @disable`
        void opOpAssign(string op, Us...)(Us values) if (op == "~" &&
                                                         values.length >= 1 &&
                                                         allSatisfy!(isElementAssignable, Us))
        {
            pushBack(values);
        }
	void opOpAssign(string op, R)(R values) if (op == "~" &&
                                                    isInputRange!R &&
                                                    allSatisfy!(isElementAssignable, ElementType!R))
        {
            pushBack(values);
        }
	void opOpAssign(string op, A)(const ref A values) if (op == "~" &&
                                                              isThisArray!A &&
                                                              isElementAssignable!(ElementType!A)) { pushBack(values); }
    }

    static if (IsOrdered!ordering)
    {
        import std.range : SearchPolicy;
        import std.range : assumeSorted;

        /// Returns: `true` iff this contains `value`.
        bool contains(U)(U value) const nothrow @nogc @("complexity", "O(log(length))")
        {
            return this[].contains(value);
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
                assert(sort([values]).findAdjacent.empty, "Parameter `values` must not contain duplicate elements");
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
                    const initialLength = length;
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
                        const ix = length - expandedLength;
                        completeSort!comp(ptr[0 .. ix].assumeSorted!comp,
                                          ptr[ix .. length]);
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
                    const ix = length - values.length;
                    completeSort!comp(ptr[0 .. ix].assumeSorted!comp,
                                      ptr[ix .. length]);
                }
            }
        }
        alias linsert = linearInsert;
    }
    else
    {
        /** Insert element(s) `values` at array offset `index`. */
        void linearInsertAtIndex(Us...)(size_t index, Us values) nothrow @("complexity", "O(length)")
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            linearInsertAtIndexHelper(index, values);
        }

        /** Insert element(s) `values` at the beginning. */
        void linearPushFront(Us...)(Us values) nothrow @("complexity", "O(length)")
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
        reserve(length + values.length);

        // TODO factor this to robustCopy. It uses copy when no overlaps (my algorithm_em), iteration otherwise
        enum usePhobosCopy = false;
        static if (usePhobosCopy)
        {
            // TODO why does this fail?
            import std.algorithm.mutation : copy;
            copy(ptr[index ..
                     length],        // source
                 ptr[index + values.length ..
                     length + values.length]); // target
        }
        else
        {
            // move second part in reverse
            // TODO functionize move
            foreach (const i; 0 .. length - index) // each element index that needs to be moved
            {
                const si = length - 1 - i; // source index
                const ti = si + values.length; // target index
                ptr[ti] = ptr[si]; // TODO move construct?
            }
        }

        // set new values
        foreach (const i, ref value; values)
        {
            ptr[index + i] = value; // TODO use range algorithm instead?
        }

        _length += values.length;
    }

    private void pushBackHelper(Us...)(Us values) @trusted nothrow @("complexity", "O(1)")
    {
        reserve(length + values.length);
        size_t i = 0;
        foreach (ref value; values)
        {
            ptr[length + i] = value;
            ++i;
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
        auto opSlice()
        {
            return opSlice!(typeof(this))(0, _length);
        }
        /// ditto
        auto opSlice(this This)(size_t i, size_t j) @trusted // const because mutation only via `op.*Assign`
        {
            alias ET = ContainerElementType!(This, E);
            import std.range : assumeSorted;
            return (cast(const(ET)[])slice[i .. j]).assumeSorted!comp;
        }

        auto ref opIndex(size_t i) @trusted
        {
            alias ET = ContainerElementType!(typeof(this), E);
            return cast(const(ET))slice[i];
        }
    }
    else
    {
    inout nothrow @nogc:                  // indexing and slicing can be mutable when ordered

        /// Slice operator overload is mutable when unordered.
        auto opSlice()
        {
            return this.opSlice(0, _length);
        }
        /// ditto
        auto opSlice(this This)(size_t i, size_t j) @trusted
        {
            alias ET = ContainerElementType!(This, E);
            return cast(inout(ET)[])slice[i .. j];
        }

        /// Index operator can be const or mutable when unordered.
        auto ref opIndex(size_t i) @trusted
        {
            alias ET = ContainerElementType!(typeof(this), E);
            return cast(inout(ET))slice[i];
        }
    }

    /// Get front element.
    inout(E) front() inout @trusted
    {
        assert(!empty); // if (empty) { throw new RangeError(); }
        return ptr[0];
    }

    /// Get back element.
    inout(E) back() inout @trusted
    {
        assert(!empty); // if (empty) { throw new RangeError(); }
        return ptr[_length - 1];
    }

    pure nothrow @nogc:

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
    alias opDollar = length;    ///< ditto

    /// Shrink length to `length`.
    void shrinkTo(size_t length) @safe
    {
        assert(length <= _length);
        _length = length;
    }
    alias opDollar = length;    ///< ditto

    /// Get length of reserved store.
    size_t reservedLength() const @safe
    {
        return _storeCapacity;
    }

    /// Get internal pointer.
    private inout(E*) ptr() inout
    {
        // TODO Use cast(ET[])?: alias ET = ContainerElementType!(typeof(this), E);
        return _storePtr;
    }

    /// Get internal slice.
    private auto ref slice() inout @trusted
    {
        return ptr[0 .. length];
    }

private:
    // TODO reuse Store store
    E* _storePtr;               // store pointer
    size_t _storeCapacity;      // store capacity
    size_t _length;             // length
}

static void tester(Ordering ordering, bool supportGC, alias less)()
{
    import std.functional : binaryFun;
    import std.range : iota, retro, chain, repeat, only, ElementType;
    import std.algorithm : filter, map;
    import std.algorithm.sorting : isSorted, sort;
    import std.exception : assertThrown, assertNotThrown;
    import std.traits : isInstanceOf;

    alias comp = binaryFun!less; //< comparison

    alias E = int;
    alias A = Array;

    foreach (Ch; AliasSeq!(char, wchar, dchar))
    {
        alias Str = A!(Ch, ordering, supportGC, less);
        Str str;
        static assert(is(ElementType!Str == Ch));
        static assert(str.isString);
    }

    static if (E.sizeof == 4)
    {
        foreach (n; [0, 1, 2, 3, 4])
        {
            assert(A!(E, ordering, supportGC, less)(n).isSmall);
        }
        assert(!(A!(E, ordering, supportGC, less)(5).isSmall));
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

        A!(E, ordering, supportGC, less) ss0 = bw; // reversed
        static assert(is(ElementType!(typeof(ss0)) == E));
        static assert(isInstanceOf!(Array, typeof(ss0)));
        assert(ss0.length == n);

        static if (IsOrdered!ordering)
        {
            if (!ss0.empty) { assert(ss0[0] == ss0[0]); } // trigger use of opindex
            assert(ss0[].equal(fw.array.sort!comp));
            assert(ss0[].isSorted!comp);
        }

        A!(E, ordering, supportGC, less) ss1 = fw; // ordinary
        assert(ss1.length == n);

        static if (IsOrdered!ordering)
        {
            assert(ss1[].equal(fw.array.sort!comp));
            assert(ss1[].isSorted!comp);
        }

        A!(E, ordering, supportGC, less) ss2 = fw.filter!(x => x & 1);
        assert(ss2.length == n/2);

        static if (IsOrdered!ordering)
        {
            assert(ss2[].equal(fw.filter!(x => x & 1).array.sort!comp));
            assert(ss2[].isSorted!comp);
        }

        auto ss32 = A!(E, ordering, supportGC, less)(32);

        auto ssA = A!(E, ordering, supportGC, less)(0);
        static if (IsOrdered!ordering)
        {
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

            auto ssB = A!(E, ordering, supportGC, less)(0);
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
            assert(ssA.ptr is null);

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
            assert(ssA.ptr is null);

            // linearPopAtIndex
            ssA ~= 1;
            ssA ~= 2;
            ssA ~= 3;
            ssA ~= 4;
            ssA ~= 5;
            assertNotThrown(ssA.linearPopAtIndex(2));
            assert(ssA[].equal([1, 2, 4, 5]));

            // pushBack and assignment from slice
            auto ssB = A!(E, ordering, supportGC, less)(0);
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
                A!(E, ordering, supportGC, less) s1 = s;
                A!(E, ordering, supportGC, less) s2 = s1[];
                assert(s1[].equal(s));
                s1 ~= s1;
                assert(s1[].equal(chain(s, s)));
                s1 ~= s2;
                assert(s1[].equal(chain(s, s, s)));
            }

            auto ssC = A!(E, ordering, supportGC, less)(0);
            const(int)[] i5 = [1, 2, 3, 4, 5];
            ssC.pushBack(i5);
            assert(ssC[].equal(i5));

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

            ssC.clear();
            assert(ssC.empty);
        }
    }
}

/// use GC
pure nothrow unittest
{
    import std.traits : EnumMembers;
    foreach (ordering; EnumMembers!Ordering)
    {
        tester!(ordering, true, "a < b"); // use GC
        tester!(ordering, true, "a > b"); // use GC
    }
}

/// don't use GC
pure nothrow /+TODO @nogc+/ unittest
{
    import std.traits : EnumMembers;
    foreach (ordering; EnumMembers!Ordering)
    {
        tester!(ordering, false, "a < b"); // don't use GC
        tester!(ordering, false, "a > b"); // don't use GC
    }
}

template ContainerElementType(ContainerType, ElementType)
{
    import std.traits : isMutable, hasIndirections, PointerTarget, isPointer, Unqual;

    template ET(bool isConst, T)
    {
        static if (isPointer!ElementType)
        {
            enum PointerIsConst = is(ElementType == const);
            enum PointerIsImmutable = is(ElementType == immutable);
            enum DataIsConst = is(PointerTarget!ElementType == const);
            enum DataIsImmutable = is(PointerTarget!ElementType == immutable);
            static if (isConst)
            {
                static if (PointerIsConst)
                    alias ET = ElementType;
                else static if (PointerIsImmutable)
                    alias ET = ElementType;
                else
                    alias ET = const(PointerTarget!ElementType)*;
            }
            else
            {
                static assert(DataIsImmutable, "An immutable container cannot reference const or mutable data");
                static if (PointerIsConst)
                    alias ET = immutable(PointerTarget!ElementType)*;
                else
                    alias ET = ElementType;
            }
        }
        else
        {
            static if (isConst)
            {
                static if (is(ElementType == immutable))
                    alias ET = ElementType;
                else
                    alias ET = const(Unqual!ElementType);
            }
            else
                alias ET = immutable(Unqual!ElementType);
        }
    }

    static if (isMutable!ContainerType)
        alias ContainerElementType = ElementType;
    else
    {
        static if (hasIndirections!ElementType)
            alias ContainerElementType = ET!(is(ContainerType == const), ElementType);
        else
            alias ContainerElementType = ElementType;
    }
}
