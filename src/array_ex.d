/** Array container(s) with optional sortedness via template-parameter
    `Ordering` and optional use of GC via `useGCAllocation`.

    TODO Remove _isLarge and code it into top-most bit of length instead

    TODO Replace ` = void` with construction or emplace

    TODO Breakout common logic into private `BasicArray` and reuse with `alias this` to express StandardArray, SortedArray, SortedSetArray

    TODO Use std.array.insertInPlace in insert()?
    TODO Use std.array.replaceInPlace?

    TODO Use `std.algorithm.mutation.move` and `std.range.primitives.moveAt`
    when moving internal sub-slices

    TODO Add `c.insertAfter(r, x)` where `c` is a collection, `r` is a range
    previously extracted from `c`, and `x` is a value convertible to
    collection's element type. See also:
    https://forum.dlang.org/post/n3qq6e$2bis$1@digitalmars.com

    TODO All Array with const members and movals

    TODO replace qcmeman with std.experimental.allocator parameter defaulting to
    `Mallocator`

    TODO use `intsort.radixSort` when element type `isSigned` or `isUnsigned` and
    above or below a certain threshold calculated by my benchmarks

    TODO Remove explicit moves when DMD std.algorithm.mutation.move calls these
    members for us (if they exist)
 */
module array_ex;

// version = useMemoryErrorHandler;
version(useMemoryErrorHandler) unittest
{
    import etc.linux.memoryerror : registerMemoryErrorHandler;
    registerMemoryErrorHandler();
    import std.stdio : writeln;
    writeln("registerMemoryErrorHandler done");
}
// version = showCtors;

/** Returns: statically (stack) allocated array with elements of type `T` of
    length `n`.

    For more convenient usage alias it as `s' together with UFCS for the
    following convenient notation:

    const x = [1, 2, 3].s;

    TODO Add to Phobos `std.array`.
*/
T[length] asStatic(T, size_t length)(T[length] arr)
    @safe pure nothrow @nogc // TODO remove. needed for now in order for DIP-1000 to work correctly, See http://forum.dlang.org/post/otrsanpgmokzpzqmfyvx@forum.dlang.org
{
    return arr;
}

///
@safe pure nothrow @nogc unittest
{
    auto x = [1, 2, 3].asStatic;
    static assert(is(typeof(x) == int[x.length]));
    static assert(is(typeof([1, 2, 3].asStatic) == int[x.length]));
}

enum Ordering
{
    unsorted, // unsorted array
    sortedValues, // sorted array with possibly duplicate values
    sortedUniqueSet, // sorted array with unique values
}

enum IsOrdered(Ordering ordering) = ordering != Ordering.unsorted;

version(unittest)
{
    import std.conv : to;
    import std.traits : Unqual;
    import std.algorithm.comparison : equal;
    import std.meta : AliasSeq;
    import dbgio : dln;
}

import container_traits : ContainerElementType;

import std.traits : isInstanceOf;
import traits_ex : isCopyable;

/// Is `true` iff `C` is an instance of an `Array` container.
enum isArrayContainer(C) = isInstanceOf!(Array, C);

/// Semantics of copy construction and copy assignment.
enum Assignment
{
    disabled,           /// for reference counting use `std.typecons.RefCounted`. for safe slicing use `borrown`
    move,               /// only move construction allowed
    copy                /// always copy (often not the desirable)
}

/** Array of value types `E` with optional sortedness/ordering.

    Always `@safe pure nothrow @nogc` when possible.

    `Assignment` either
    - is disabled
    - does Rust-style move, or
    - does C++-style copying

    Params:
        useGCAllocation = `true` iff `GC.malloc` is used for store allocation,
                          otherwise C's `{m,ce,re}alloc()` is used.
 */
private struct Array(E,
                     // TODO merge these flags into one to reduce template bloat
                     Assignment assignment = Assignment.disabled,
                     Ordering ordering = Ordering.unsorted,
                     bool useGCAllocation = false,
                     CapacityType = size_t, // see also https://github.com/izabera/s
                     alias less = "a < b") // TODO move out of this definition and support only for the case when `ordering` is not `Ordering.unsorted`
    if (is(CapacityType == size_t) ||
        is(CapacityType == uint))
{
    import std.conv : emplace;
    import std.range : isInputRange, isIterable, ElementType;
    import std.traits : isAssignable, Unqual, isSomeChar, isArray, isScalarType, hasElaborateDestructor, TemplateOf;
    import std.functional : binaryFun;
    import std.meta : allSatisfy;
    import core.stdc.string : memset;
    import std.algorithm.mutation : move, moveEmplace, moveEmplaceAll;
    import qcmeman : malloc, calloc, realloc, free, gc_addRange, gc_removeRange;

    /// Mutable element type.
    private alias MutableE = Unqual!E;

    /// Type of `this`.
    private alias This = typeof(this);

    /// Template for type of `this`.
    private alias ThisTemplate = TemplateOf!(This);

    /// Same type as this but with mutable element type.
    private alias MutableThis = ThisTemplate!(MutableE, assignment, ordering, useGCAllocation, CapacityType, less);

    private template shouldAddGCRange(T)
    {
        import std.traits : hasIndirections, isInstanceOf;
        enum shouldAddGCRange = hasIndirections!T && !isInstanceOf!(Array, T); // TODO unify to container_traits.shouldAddGCRange
    }

    static if (useGCAllocation || // either we asked for allocation
               shouldAddGCRange!E) // or we need GC ranges
    {
        import core.memory : GC;
    }

    /// Type of element stored.
    // alias ElementType = E;

    /// Is `true` iff `Array` can be interpreted as a D `string`, `wstring` or `dstring`.
    enum isString = isSomeChar!E;

    alias comp = binaryFun!less; //< comparison

    /// Create a empty array.
    this(typeof(null)) nothrow
    {
        version(showCtors) dln("ENTERING: smallCapacity:", smallCapacity, " @",  __PRETTY_FUNCTION__);
        // nothing needed, rely on default initialization of data members
    }

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    pragma(inline)
    static This withLength(size_t initialLength) @trusted nothrow
    {
        version(showCtors) dln("ENTERING: smallCapacity:", smallCapacity, " @",  __PRETTY_FUNCTION__);

        debug typeof(return) that;
        else typeof(return) that = void;

        // TODO functionize:
        if (initialLength > smallCapacity)
        {
            emplace!Large(&that._large, initialLength, initialLength, true); // no elements so we need to zero
        }
        else
        {
            that._small.length = cast(ubyte)initialLength;
        }

        version(showCtors) dln("EXITING: ", __PRETTY_FUNCTION__);
        return that;
    }

    /// Returns: an array with initial capacity `initialCapacity`.
    pragma(inline)
    static This withCapacity(size_t initialCapacity) @trusted nothrow
    {
        version(showCtors) dln("ENTERING: smallCapacity:", smallCapacity, " @",  __PRETTY_FUNCTION__);

        debug typeof(return) that;
        else typeof(return) that = void;

        if (initialCapacity > smallCapacity)
        {
            emplace!Large(&that._large, initialCapacity, 0, false);
        }
        else
        {
            that._small.length = 0;
        }

        version(showCtors) dln("EXITING: ", __PRETTY_FUNCTION__);
        return that;
    }

    /// Returns: an array of one element `element`.
    pragma(inline)
    static This withElement(E element) @trusted nothrow
    {
        version(showCtors) dln("ENTERING: smallCapacity:", smallCapacity, " @",  __PRETTY_FUNCTION__);

        debug typeof(return) that;
        else typeof(return) that = void;

        // TODO functionize:
        enum initialLength = 1;
        if (initialLength > smallCapacity)
        {
            emplace!Large(&that._large, initialLength, initialLength, false);
        }
        else
        {
            emplace!Small(&that._small, initialLength, false);
        }

        // move element
        static if (isCopyable!E)
        {
            that._mptr[0] = element;
        }
        else static if (!shouldAddGCRange!E)
        {
            moveEmplace(*(cast(MutableE*)(&element)), that._mptr[0]); // safe to cast away constness when no indirections
        }
        else
        {
            moveEmplace(element, that._mptr[0]); // TODO remove `move` when compiler does it for us
        }

        return that;
    }

    // /// Returns: an array of `Us.length` number of elements set to `elements`.
    pragma(inline)
    static This withElements(Us...)(Us elements) @trusted nothrow
    {
        version(showCtors) dln("ENTERING: smallCapacity:", smallCapacity, " @",  __PRETTY_FUNCTION__);

        debug typeof(return) that;
        else typeof(return) that = void;

        // TODO functionize:
        enum initialLength = Us.length;
        if (initialLength > smallCapacity)
        {
            emplace!Large(&that._large, initialLength, initialLength, false);
        }
        else
        {
            emplace!Small(&that._small, initialLength, false);
        }

        // move elements
        foreach (immutable i, ref element; elements)
        {
            static if (!shouldAddGCRange!E)
            {
                moveEmplace(*(cast(MutableE*)(&element)), that._mptr[i]); // safe to cast away constness when no indirections
            }
            else
            {
                moveEmplace(element, that._mptr[i]); // TODO remove `move` when compiler does it for us
            }
        }

        return that;
    }

    static if (assignment == Assignment.copy)
    {
        /// Copy construction.
        this(this) nothrow @trusted
        {
            version(showCtors) dln("Copy ctor: ", This.stringof);
            if (isLarge)        // only large case needs special treatment
            {
                auto rhs_storePtr = _large.ptr; // save store pointer
                _large.setCapacity(this.length); // pack by default
                // _large.length already copied
                _large.ptr = allocate(this.length, false);
                foreach (immutable i; 0 .. this.length)
                {
                    _large.ptr[i] = rhs_storePtr[i];
                }
            }
        }

        /// Copy assignment.
        void opAssign(This rhs) @trusted
        {
            version(showCtors) dln("Copy assign: ", This.stringof);
            // self-assignment may happen when assigning derefenced pointer
            if (isLarge)        // large = ...
            {
                if (rhs.isLarge) // large = large
                {
                    // TODO functionize to Large.opAssign(Large rhs):
                    if (_large.ptr != rhs._large.ptr) // if not self assignment
                    {
                        _large.length = rhs._large.length;
                        reserve(rhs._large.length);
                        foreach (immutable i; 0 .. rhs._large.length)
                        {
                            _large.ptr[i] = rhs._large.ptr[i];
                        }
                    }
                }
                else            // large = small
                {
                    {            // make it small
                        clear();    // clear large storage
                        _large.isLarge = false; // TODO needed?
                    }
                    _small = rhs._small; // small
                }
            }
            else                // small = ...
            {
                if (rhs.isLarge) // small = large
                {
                    {            // make it large
                        clear(); // clear small storage
                        _large.isLarge = true; // TODO needed?
                    }
                    // TODO functionize to Large.opAssign(Large rhs):
                    if (_large.ptr != rhs._large.ptr) // if not self assignment
                    {
                        _large.length = rhs._large.length;
                        reserve(rhs._large.length);
                        foreach (immutable i; 0 .. rhs._large.length)
                        {
                            _large.ptr[i] = rhs._large.ptr[i];
                        }
                    }
                }
                else            // small = small
                {
                    _small = rhs._small;
                }
            }
        }
    }
    else static if (assignment == Assignment.disabled)
    {
        @disable this(this);
    }
    else static if (assignment == Assignment.move)
    {
        /// Copy ctor moves.
        this(This rhs) @trusted
        {
            version(showCtors) dln("Copying: ", This.stringof);
            assert(!isBorrowed);
            moveEmplace(rhs, this); // TODO remove `move` when compiler does it for us
        }

        /// Assignment moves.
        void opAssign(This rhs) @trusted
        {
            assert(!isBorrowed);
            import std.algorith.mutation : move;
            move(rhs, this);  // TODO remove `move` when compiler does it for us
        }
    }

    static if (isCopyable!E)
    {
        /// Returns: shallow duplicate of `this`.
        @property MutableThis dup() const @trusted // `MutableThis` mimics behaviour of `dup` for builtin D arrays
        {
            debug typeof(return) that;
            else typeof(return) that = void;
            if (isLarge)
            {
                emplace!(that.Large)(&that._large, _large.length, _large.length, false);
                foreach (immutable i; 0 .. _large.length)
                {
                    that._large.ptr[i] = _large.ptr[i];
                }
            }
            else
            {
                emplace!(that.Small)(&that._small, _small.length, false);
                that._small.elms[0 .. _small.length] = _small.elms[0 .. _small.length]; // copy elements
            }
            return that;
        }
    }

    bool opEquals(const ref This rhs) const @trusted
    {
        static if (isCopyable!E)
        {
            return this[] == rhs[]; // TODO fix DMD to make this work for non-copyable aswell
        }
        else
        {
            if (this.length != rhs.length) { return false; }
            foreach (immutable i; 0 .. this.length)
            {
                if (this.ptr[i] != rhs.ptr[i]) { return false; }
            }
            return true;
        }
    }
    bool opEquals(in This rhs) const @trusted
    {
        static if (isCopyable!E)
        {
            return this[] == rhs[]; // TODO fix DMD to make this work for non-copyable aswell
        }
        else
        {
            if (this.length != rhs.length) { return false; }
            foreach (immutable i; 0 .. this.length)
            {
                if (this.ptr[i] != rhs.ptr[i]) { return false; }
            }
            return true;
        }
    }

    /// Compare with range `R` with comparable element type.
    pragma(inline, true)
    bool opEquals(R)(R rhs) const
        // TODO do we need to restrict this?: if (isInputRange!R)
    {
        return opSlice.equal(rhs);
    }

    /// Calculate D associative array (AA) key hash.
    size_t toHash() const @trusted pure nothrow
    {
        import core.internal.hash : hashOf;
        // TODO this doesn't work when element type is non-copyable: return this.slice.hashOf;
        typeof(return) hash = this.length;
        foreach (immutable i; 0 .. this.length)
        {
            hash ^= this.ptr[i].hashOf;
        }
        return hash;
    }

    /** Construct from InputRange `values`.
        If `values` are sorted `assumeSortedParameter` is `true`.

        TODO Have `assumeSortedParameter` only when `IsOrdered!ordering` is true
     */
    this(R)(R values, bool assumeSortedParameter = false) @trusted @("complexity", "O(n*log(n))")
        if (isIterable!R)
    {
        version(showCtors) dln("ENTERING: smallCapacity:", smallCapacity, " @",  __PRETTY_FUNCTION__);

        // append new data
        import std.range.primitives : hasLength;
        static if (hasLength!R)
        {
            // TODO choose large or small depending on values.length
            _small.isLarge = false;
            _small.length = 0;

            reserve(values.length); // fast reserve
            setOnlyLength(values.length);

            size_t i = 0;
            foreach (ref value; move(values)) // TODO remove `move` when compiler does it for us
            {
                _mptr[i++] = value;
            }
        }
        else
        {
            // always start small
            _small.isLarge = false;
            _small.length = 0;

            size_t i = 0;
            foreach (ref value; move(values)) // TODO remove `move` when compiler does it for us
            {
                reserve(i + 1); // slower reserve
                _mptr[i++] = value;
                setOnlyLength(i); // must be set here because correct length is needed in reserve call above in this same scope
            }
        }

        static if (IsOrdered!ordering)
        {
            if (!assumeSortedParameter)
            {
                import std.algorithm.sorting : sort;
                sort!comp(_mptr[0 .. this.length]);
            }
        }

        version(showCtors) dln("EXITING: ", __PRETTY_FUNCTION__);
    }

    /// Reserve room for `newCapacity`.
    void reserve(size_t newCapacity) pure nothrow @trusted
    {
        assert(!isBorrowed);
        if (newCapacity <= capacity) { return; }
        if (isLarge)
        {
            static if (shouldAddGCRange!E)
            {
                gc_removeRange(_mptr);
            }
            import std.math : nextPow2;
            reallocateLargeStoreAndSetCapacity(newCapacity.nextPow2);
            static if (shouldAddGCRange!E)
            {
                gc_addRange(_mptr, _large.capacity * E.sizeof);
            }
        }
        else
        {
            if (newCapacity > smallCapacity) // convert to large
            {
                auto tempLarge = Large(newCapacity, length, false);

                // move small to temporary large
                foreach (immutable i; 0 .. length)
                {
                    moveEmplace(_small._mptr[i],
                                tempLarge._mptr[i]);
                }

                static if (hasElaborateDestructor!E)
                {
                    destroyElements();
                }

                // TODO functionize:
                {               // make this large
                    moveEmplace(tempLarge, _large);
                }

                assert(isLarge);
            }
            else
            {
                // staying small
            }
        }
    }

    /// Pack/Compress storage.
    void compress() pure nothrow @trusted
    {
        assert(!isBorrowed);
        if (isLarge)
        {
            if (this.length)
            {
                if (this.length <= smallCapacity)
                {
                    Small tempSmall = Small(length, false);

                    // move elements to temporary small. TODO make moveEmplaceAll work on char[],char[] and use
                    foreach (immutable i; 0 .. length)
                    {
                        moveEmplace(_large._mptr[i], tempSmall._mptr[i]);
                    }

                    // free existing large data
                    static if (shouldAddGCRange!E)
                    {
                        gc_removeRange(_mptr);
                    }
                    static if (useGCAllocation)
                    {
                        GC.free(_mptr);
                    }
                    else
                    {
                        free(_mptr);
                    }

                    moveEmplace(tempSmall, _small);
                }
                else
                {
                    if (_large.capacity != this.length)
                    {
                        static if (shouldAddGCRange!E)
                        {
                            gc_removeRange(_mptr);
                        }
                        reallocateLargeStoreAndSetCapacity(this.length);
                        static if (shouldAddGCRange!E)
                        {
                            gc_addRange(_mptr, _large.capacity * E.sizeof);
                        }
                    }
                }
            }
            else                // if empty
            {
                // free data
                static if (shouldAddGCRange!E)
                {
                    gc_removeRange(_mptr);
                }
                static if (useGCAllocation)
                {
                    GC.free(_mptr);
                }
                else
                {
                    free(_mptr);
                }
                _large.capacity = 0;
                _large.ptr = null;
            }
        }
    }
    /// ditto
    alias pack = compress;

    /// Reallocate storage. TODO move to Large.reallocateAndSetCapacity
    pragma(inline, true)
    private void reallocateLargeStoreAndSetCapacity(size_t newCapacity) pure nothrow @trusted
    {
        _large.setCapacity(newCapacity);
        static if (useGCAllocation)
        {
            _large.ptr = cast(E*)GC.realloc(_mptr, E.sizeof * _large.capacity);
        }
        else                    // @nogc
        {
            _large.ptr = cast(E*)realloc(_mptr, E.sizeof * _large.capacity);
            assert(_large.ptr, "Reallocation failed");
        }
    }

    /// Destruct.
    pragma(inline, true)
    ~this() nothrow @trusted
    {
        assert(!isBorrowed);
        if (isLarge)
        {
            debug assert(_large.ptr != _ptrMagic, "Double free."); // trigger fault for double frees
        }
        release();
        if (isLarge)
        {
            debug _large.ptr = _ptrMagic; // tag as freed
        }
    }

    /// Empty.
    pragma(inline, true)
    void clear() @safe nothrow
    {
        assert(!isBorrowed);
        release();
        resetInternalData();
    }
    /// ditto
    alias makeEmpty = clear;
    /// ditto
    pragma(inline, true)
    void opAssign(typeof(null)) @safe{ clear(); }

    /// Destroy elements.
    static if (hasElaborateDestructor!E)
    {
        private void destroyElements() nothrow @trusted
        {
            foreach (immutable i; 0 .. this.length)
            {
                .destroy(_mptr[i]);
            }
        }
    }

    /// Release internal store.
    private void release() nothrow @trusted
    {
        static if (hasElaborateDestructor!E)
        {
            destroyElements();
        }
        if (isLarge)
        {
            static if (shouldAddGCRange!E)
            {
                gc_removeRange(_large.ptr);
            }
            static if (useGCAllocation)
            {
                GC.free(_large.ptr);
            }
            else                // @nogc
            {
                static if (!shouldAddGCRange!E)
                {
                    free(cast(MutableE*)_large.ptr); // safe to case away constness
                }
                else
                {
                    free(_large.ptr);
                }
            }
        }
    }

    /// Reset internal data.
    pragma(inline, true)
    private void resetInternalData() @trusted pure nothrow @nogc
    {
        if (isLarge)
        {
            _large.ptr = null;
            _large.length = 0;
            _large.capacity = 0;
        }
        else
        {
            _small.length = 0; // fast discardal
        }
    }

    /// Is `true` if `U` can be assign to the element type `E` of `this`.
    enum isElementAssignable(U) = isAssignable!(E, U);

    /** Removal doesn't need to care about ordering. */
    ContainerElementType!(This, E) popAtIndex(size_t index) @trusted @("complexity", "O(length)")
    {
        assert(!isBorrowed);
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

        decOnlyLength();
        return move(value); // TODO remove `move` when compiler does it for us
    }
    alias removeAt = popAtIndex;
    alias deleteAt = popAtIndex;

    /** Removal doesn't need to care about ordering. */
    pragma(inline, true)
    ContainerElementType!(This, E) linearPopFront() @trusted @("complexity", "O(length)")
    {
        return popAtIndex(0);
    }

    /** Removal doesn't need to care about ordering. */
    pragma(inline, true)
    void popBack() @safe @("complexity", "O(1)")
    {
        assert(!isBorrowed);
        assert(!empty);
        decOnlyLength();
    }

    /** Pop back element and return it. */
    pragma(inline, true)
    E backPop() @trusted
    {
        assert(!isBorrowed);
        assert(!empty);
        decOnlyLength();
        import std.traits : hasIndirections;
        static if (hasIndirections!E) // TODO better trait?
        {
            return move(_mptr[this.length]); // move is indeed need here
        }
        else
        {
            return _mptr[this.length]; // no move needed
        }
    }
    alias stealBack = backPop;

    /** Pop last `count` back elements. */
    pragma(inline, true)
    void popBackN(size_t count) @safe @("complexity", "O(1)")
    {
        assert(!isBorrowed);
        shrinkTo(this.length - count);
    }

    static if (!IsOrdered!ordering) // for unsorted arrays
    {
        /// Push back (append) `values`.
        pragma(inline) void pushBack(Us...)(Us values) @("complexity", "O(1)") @trusted
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            assert(!isBorrowed);
            immutable newLength = this.length + values.length;
            reserve(newLength);
            foreach (immutable i, ref value; values) // `ref` so we can `move`
            {
                static if (isScalarType!(typeof(value)))
                {
                    _mptr[this.length + i] = value;
                }
                else
                {
                    moveEmplace(value, _mptr[this.length + i]); // TODO remove `move` when compiler does it for us
                }
            }
            setOnlyLength(this.length + values.length);
        }
        /// ditto
        void pushBack(R)(R values) @("complexity", "O(values.length)")
            if (isInputRange!R &&
                !(isArray!R) &&
                !(isArrayContainer!R) &&
                isElementAssignable!(ElementType!R))
        {
            assert(!isBorrowed);
            reserve(this.length + values.length);
            foreach (immutable i, ref value; values) // `ref` so we can `move`
            {
                static if (isScalarType!(typeof(value)))
                {
                    _mptr[this.length + i] = value;
                }
                else
                {
                    moveEmplace(value, _mptr[this.length + i]); // TODO remove `moveEmplace` when compiler does it for us
                }
            }
            setOnlyLength(this.length + values.length);
        }
        /// ditto.
        void pushBack(A)(A values) @trusted @("complexity", "O(values.length)")
            if (isArray!A &&
                isElementAssignable!(ElementType!A))
        {
            assert(!isBorrowed);
            if (ptr == values.ptr) // called as: this ~= this. TODO extend to check if `values` overlaps ptr[0 .. _large.capacity]
            {
                reserve(2*this.length);
                foreach (immutable i; 0 .. this.length)
                {
                    _mptr[this.length + i] = ptr[i]; // needs copying
                }
                setOnlyLength(2 * this.length);
            }
            else
            {
                reserve(this.length + values.length);
                if (is(MutableE == Unqual!(ElementType!A)))
                {
                    // TODO reuse memcopy and no overlap
                }
                foreach (immutable i, ref value; values)
                {
                    _mptr[this.length + i] = value;
                }
                setOnlyLength(this.length + values.length);
            }
        }
        /// ditto.
        void pushBack(A)(const ref A values) @trusted @("complexity", "O(values.length)") // TODO `in` parameter qualifier doesn't work here. Compiler bug?
            if (isArrayContainer!A &&
                isElementAssignable!(ElementType!A))
        {
            assert(!isBorrowed);
            if (ptr == values.ptr) // called as: this ~= this
            {
                reserve(2*this.length);
                // NOTE: this is not needed because we don't need range checking here?:
                // _mptr[length .. 2*length] = values.ptr[0 .. length];
                foreach (immutable i; 0 .. this.length)
                {
                    _mptr[this.length + i] = values.ptr[i];
                }
                setOnlyLength(2 * this.length);
            }
            else
            {
                reserve(this.length + values.length);
                if (is(MutableE == Unqual!(ElementType!A)))
                {
                    // TODO reuse memcopy and no overlap
                }
                foreach (immutable i, ref value; values.slice)
                {
                    _mptr[this.length + i] = value;
                }
                setOnlyLength(this.length + values.length);
            }
        }
        alias append = pushBack;
        alias put = pushBack;   // OutputRange support

        pragma(inline, true):

        // NOTE these separate overloads of opOpAssign are needed because one
        // `const ref`-parameter-overload doesn't work because of compiler bug
        // with: `this(this) @disable`
        void opOpAssign(string op, Us...)(Us values)
            if (op == "~" &&
                values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            assert(!isBorrowed);
            pushBack(move(values)); // TODO remove `move` when compiler does it for us
        }
        void opOpAssign(string op, R)(R values)
            if (op == "~" &&
                isInputRange!R &&
                allSatisfy!(isElementAssignable, ElementType!R))
        {
            assert(!isBorrowed);
            pushBack(move(values)); // TODO remove `move` when compiler does it for us
        }
        void opOpAssign(string op, A)(const ref A values)
            if (op == "~" &&
                isArrayContainer!A &&
                isElementAssignable!(ElementType!A))
        {
            assert(!isBorrowed);
            pushBack(values);
        }
    }

    import searching_ex : containsStoreIndex; // TODO this is redundant but elides rdmd dependency error from array_ex.d

    static if (IsOrdered!ordering)
    {
        import std.range : SearchPolicy, assumeSorted;

        /// Returns: `true` iff this contains `value`.
        pragma(inline, true) bool contains(U)(U value) const nothrow @nogc @("complexity", "O(log(length))")
        {
            return this[].contains(value); // reuse `SortedRange.contains`
        }

        /** Wrapper for `std.range.SortedRange.lowerBound` when this `ordering` is sorted. */
        pragma(inline, true) auto lowerBound(SearchPolicy sp = SearchPolicy.binarySearch, U)(U e) inout @("complexity", "O(log(length))")
        {
            return this[].lowerBound!sp(e); // reuse `SortedRange.lowerBound`
        }

        /** Wrapper for `std.range.SortedRange.upperBound` when this `ordering` is sorted. */
        pragma(inline, true) auto upperBound(SearchPolicy sp = SearchPolicy.binarySearch, U)(U e) inout @("complexity", "O(log(length))")
        {
            return this[].upperBound!sp(e); // reuse `SortedRange.upperBound`
        }

        static if (ordering == Ordering.sortedUniqueSet)
        {
            /** Inserts several `values` into `this` ordered set.

                Returns: `bool`-array with same length as `values`, where i:th
                `bool` value is set if `value[i]` wasn't previously in `this`.
            */
            bool[Us.length] insertMany(SearchPolicy sp = SearchPolicy.binarySearch, Us...)(Us values) @("complexity", "O(length)")
                if (values.length >= 1 &&
                    allSatisfy!(isElementAssignable, Us))
            in
            {
                assert(!isBorrowed);

                // assert no duplicates in `values`
                import std.range : empty;
                import std.algorithm.searching : findAdjacent;
                import std.algorithm.sorting : sort;

                // TODO functionize or use other interface in pushing `values`
                import std.traits : CommonType;
                CommonType!Us[Us.length] valuesArray;
                foreach (immutable i, const ref value; values)
                {
                    valuesArray[i] = value;
                }
                assert(sort(valuesArray[]).findAdjacent.empty, "Parameter `values` must not contain duplicate elements");
            }
            body
            {
                static if (values.length == 1) // faster because `contains()` followed by `completeSort()` searches array twice
                {
                    import searching_ex : containsStoreIndex;
                    size_t index;
                    if (slice.assumeSorted!comp.containsStoreIndex!sp(values, index)) // faster than `completeSort` for single value
                    {
                        return [false];
                    }
                    else
                    {
                        insertAtIndexHelper(index, values);
                        return [true];
                    }
                }
                else
                {
                    import std.algorithm.sorting : completeSort;

                    debug { typeof(return) hits; }
                    else  { typeof(return) hits = void; }

                    size_t expandedLength = 0;
                    immutable initialLength = this.length;
                    foreach (immutable i, ref value; values)
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
                        immutable ix = this.length - expandedLength;
                        // TODO use `_mptr` here instead and functionize to @trusted helper function
                        completeSort!comp(slice[0 .. ix].assumeSorted!comp,
                                          slice[ix .. this.length]);
                    }
                    return hits;
                }
            }
        }
        else static if (ordering == Ordering.sortedValues)
        {
            /** Inserts `values`. */
            void insertMany(SearchPolicy sp = SearchPolicy.binarySearch, Us...)(Us values) @("complexity", "O(log(length))")
                if (values.length >= 1 &&
                    allSatisfy!(isElementAssignable, Us))
            {
                assert(!isBorrowed);

                // TODO add optimization for values.length == 2
                static if (values.length == 1)
                {
                    import searching_ex : containsStoreIndex;
                    size_t index;
                    if (!slice.assumeSorted!comp.containsStoreIndex!sp(values, index)) // faster than `completeSort` for single value
                    {
                        insertAtIndexHelper(index, values);
                    }
                }
                else
                {
                    import std.algorithm.sorting : completeSort;
                    pushBackHelper(values); // simpler because duplicates are allowed
                    immutable ix = this.length - values.length;
                    completeSort!comp(_mptr[0 .. ix].assumeSorted!comp,
                                      _mptr[ix .. this.length]);
                }
            }
        }
    }
    else
    {
        /** Insert element(s) `values` at array offset `index`. */
        pragma(inline, true) void insertAtIndex(Us...)(size_t index, Us values) nothrow @("complexity", "O(length)")
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            assert(!isBorrowed);
            insertAtIndexHelper(index, values);
        }

        /** Insert element(s) `values` at the beginning. */
        pragma(inline, true) void pushFront(Us...)(Us values) nothrow @("complexity", "O(length)")
            if (values.length >= 1 &&
                allSatisfy!(isElementAssignable, Us))
        {
            insertAtIndex(0, values);
        }

        alias prepend = pushFront;
    }

    /** Helper function used externally for unsorted and internally for sorted. */
    private void insertAtIndexHelper(Us...)(size_t index, Us values) @trusted nothrow @("complexity", "O(length)")
    {
        reserve(this.length + values.length);

        // TODO factor this to robustCopy. It uses copy when no overlaps (my algorithm_em), iteration otherwise
        enum usePhobosCopy = false;
        static if (usePhobosCopy)
        {
            // TODO why does this fail?
            import std.algorithm.mutation : copy;
            copy(ptr[index ..
                     this.length],        // source
                 _mptr[index + values.length ..
                       this.length + values.length]); // target
        }
        else
        {
            // move second part in reverse
            // TODO functionize move
            foreach (immutable i; 0 .. this.length - index) // each element index that needs to be moved
            {
                immutable si = this.length - 1 - i; // source index
                immutable ti = si + values.length; // target index
                _mptr[ti] = ptr[si]; // TODO move construct?
            }
        }

        // set new values
        foreach (immutable i, ref value; values)
        {
            ptr[index + i] = value; // TODO use range algorithm instead?
        }

        setOnlyLength(this.length + values.length);
    }

    private void pushBackHelper(Us...)(Us values) @trusted nothrow @("complexity", "O(1)")
    {
        reserve(this.length + values.length);
        foreach (immutable i, ref value; values)
        {
            moveEmplace(value, _mptr[this.length + i]); // TODO remove `move` when compiler does it for us
        }
        setOnlyLength(this.length + values.length);
    }

    @property @("complexity", "O(1)")
    pragma(inline, true):

    /// ditto
    static if (IsOrdered!ordering)
    {
        const pure nothrow @nogc: // indexing and slicing must be `const` when ordered

        /// Slice operator must be const when ordered.
        auto opSlice() @safe return
        {
            return (cast(const(E)[])slice).assumeSorted!comp;
        }
        /// ditto
        auto opSlice(this This)(size_t i, size_t j) @safe return // const because mutation only via `op.*Assign`
        {
            import std.range : assumeSorted;
            return (cast(const(E)[])slice[i .. j]).assumeSorted!comp;
        }

        @trusted:

        /// Index operator must be const to preserve ordering.
        ref const(E) opIndex(size_t i) return
        {
            assert(i < this.length);
            return ptr[i];
        }

        /// Get front element (as constant reference to preserve ordering).
        ref const(E) front() return
        {
            assert(!empty);
            return ptr[0];
        }

        /// Get back element (as constant reference to preserve ordering).
        ref const(E) back() return
        {
            assert(!empty);
            return ptr[this.length - 1];
        }
    }
    else
    {
        nothrow:

        /// Set length to `newLength`.
        @property void length(size_t newLength) @safe pure
        {
            reserve(newLength);
            setOnlyLength(newLength);
        }

        @nogc:

        /// Index assign operator.
        ref E opIndexAssign(V)(V value, size_t i) @trusted return
        {
            assert(!isBorrowed);
            assert(i < this.length);
            static if (isScalarType!E)
                ptr[i] = value;
            else
                move(*(cast(MutableE*)(&value)), _mptr[i]); // TODO is this correct?
            return ptr[i];
        }

        /// Slice assign operator.
        static if (isCopyable!E)
        {
            void opSliceAssign(V)(V value, size_t i, size_t j) @trusted return
            {
                assert(!isBorrowed);
                assert(i <= j);
                assert(j <= this.length);
                foreach (immutable i; 0 .. this.length)
                {
                    ptr[i] = value;
                }
            }
        }

        pure inout: // indexing and slicing has mutable access only when unordered

        /// Slice operator.
        inout(E)[] opSlice() return
        {
            return this.opSlice(0, this.length);
        }
        /// ditto
        inout(E)[] opSlice(size_t i, size_t j) @trusted return
        {
            assert(i <= j);
            assert(j <= this.length);
            return ptr[i .. j];
        }

        @trusted:

        /// Index operator.
        ref inout(E) opIndex(size_t i) return
        {
            assert(i < this.length);
            return ptr[i];
        }

        /// Get front element reference.
        ref inout(E) front() return
        {
            assert(!empty);
            return ptr[0];
        }

        /// Get back element reference.
        ref inout(E) back() return
        {
            assert(!empty);
            return ptr[this.length - 1];
        }
    }

    alias data = opSlice;   // `std.array.Appender` compatibility

    // static if (isCopyable!E)
    // {
    //     string toString() const @property @trusted pure
    //     {
    //         import std.array : Appender;
    //         import std.conv : to;
    //         Appender!string s = "[";
    //         foreach (immutable i; 0 .. this.length)
    //         {
    //             if (i) { s.put(','); }
    //             s.put(ptr[i].to!string);
    //         }
    //         s.put("]");
    //         return s.data;
    //     }
    // }

    pure nothrow:

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

    @nogc:

    /// Check if empty.
    bool empty() const @safe { return this.length == 0; }

    /// Get length.
    size_t length() const @trusted
    {
        if (isLarge)
        {
            return _large.length;
        }
        else
        {
            return _small.length;
        }
    }
    alias opDollar = length;    /// ditto

    /// Decrease only length.
    private void decOnlyLength() @trusted
    {
        if (isLarge)
        {
            assert(_large.length);
            _large.length = _large.length - 1;
        }
        else
        {
            assert(_small.length);
            _small.length = cast(SmallLength)(_small.length - 1);
        }
    }

    /// Set only length.
    private void setOnlyLength(size_t newLength) @trusted
    {
        if (isLarge)
        {
            _large.length = newLength; // TODO compress?
        }
        else
        {
            assert(newLength <= SmallLength.max);
            _small.length = cast(SmallLength)newLength;
        }
    }

    /// Get reserved capacity of store.
    size_t capacity() const @trusted
    {
        if (isLarge)
        {
            return _large.capacity;
        }
        else
        {
            return smallCapacity;
        }
    }

    /// Shrink length to `newLength`.
    void shrinkTo(size_t newLength) @safe
    {
        assert(!isBorrowed);
        assert(newLength <= length());
        setOnlyLength(newLength);
    }

    /// Get internal pointer.
    inout(E*) ptr() inout
    {
        // TODO Use cast(ET[])?: alias ET = ContainerElementType!(This, E);
        if (isLarge)
        {
            return _large.ptr;
        }
        else
        {
            return _small.elms.ptr;
        }
    }

    /// Get internal pointer to mutable content. Doesn't need to be qualified with `scope`.
    private MutableE* _mptr() const
    {
        if (isLarge)
        {
            return _large._mptr;
        }
        else
        {
            return _small._mptr;
        }
    }

    /// Get internal slice.
    private auto slice() inout @trusted return
    {
        return ptr[0 .. this.length];
    }

    /** Magic pointer value used to detect double calls to `free`.

        Cannot conflict with return value from `malloc` because the least
        significant bit is set (when the value ends with a one).
    */
    debug private enum _ptrMagic = cast(E*)0x0C6F3C6c0f3a8471;

    /// Returns: `true` if `this` currently uses large array storage.
    bool isLarge() const @trusted // trusted access to anonymous union
    {
        assert(_large.isLarge == _small.isLarge); // must always be same
        return _large.isLarge;
    }

    /// Returns: `true` if `this` currently uses small (packed) array storage.
    bool isSmall() const @safe { return !isLarge; }

    private
    {
        private alias SmallLength = ubyte;

        private enum largeSmallLengthDifference = Large.sizeof - SmallLength.sizeof;
        private enum smallCapacity = largeSmallLengthDifference / E.sizeof;
        private enum smallPadSize = largeSmallLengthDifference - smallCapacity*E.sizeof;
    }

    /** Tag `this` borrowed.
        Used by wrapper logic in owned.d and borrowed.d
    */
    void tagAsBorrowed() @trusted
    {
        if (isLarge) { _large.isBorrowed = true; }
        else         { _small.isBorrowed = true; }
    }

    /** Tag `this` as not borrowed.
        Used by wrapper logic in owned.d and borrowed.d
    */
    void untagAsNotBorrowed() @trusted
    {
        if (isLarge) { _large.isBorrowed = false; }
        else         { _small.isBorrowed = false; }
    }

    /// Returns: `true` if this is borrowed
    bool isBorrowed() @trusted
    {
        if (isLarge) { return _large.isBorrowed; }
        else         { return _small.isBorrowed; }
    }

private:                        // data
    static struct Large
    {
        private enum lengthBits = 8*CapacityType.sizeof - 2;
        private enum lengthMax = 2^^lengthBits - 1;

        // TODO reuse andralex's module `storage` for small size/array optimization (SSO)
        static if (useGCAllocation)
            E* ptr;                // GC-allocated store pointer. See also: http://forum.dlang.org/post/iubialncuhahhxsfvbbg@forum.dlang.org
        else
            @nogc E* ptr;       // non-GC-allocated store pointer
        CapacityType capacity;  // store capacity

        import std.bitmanip : bitfields; // TODO replace with own logic cause this mixin costs compilation speed
        mixin(bitfields!(size_t, "length", lengthBits,
                         bool, "isLarge", 1,
                         bool, "isBorrowed", 1));

        this(size_t initialCapacity, size_t initialLength, bool zero)
        {
            assert(initialCapacity <= lengthMax);
            assert(initialLength <= lengthMax);

            setCapacity(initialCapacity);
            this.capacity = cast(CapacityType)initialCapacity;
            this.ptr = allocate(initialCapacity, zero);
            this.length = initialLength;
            this.isLarge = true;
            this.isBorrowed = false;
        }

        pragma(inline, true):

        void setCapacity(size_t newCapacity)
        {
            assert(newCapacity <= capacity.max);
            capacity = cast(CapacityType)newCapacity;
        }

        MutableE* _mptr() const @trusted
        {
            return cast(typeof(return))ptr;
        }
    }

    /// Small string storage.
    static struct Small
    {
        enum capacity = smallCapacity;
        private enum lengthBits = 8*SmallLength.sizeof - 2;
        private enum lengthMax = 2^^lengthBits - 1;

        E[capacity] elms;
        static if (smallPadSize)
        {
            ubyte[smallPadSize] _ignoredPadding;
        }
        import std.bitmanip : bitfields; // TODO replace with own logic cause this mixin costs compilation speed
        mixin(bitfields!(SmallLength, "length", lengthBits,
                         bool, "isLarge", 1, // defaults to false
                         bool, "isBorrowed", 1)); // default to false

        this(size_t initialLength, bool zero)
        {
            assert(initialLength <= lengthMax);

            this.length = cast(SmallLength)initialLength;
            this.isLarge = false;
            this.isBorrowed = false;
            if (zero)
            {
                elms[] = E.init;
            }
        }

        pragma(inline, true):

        MutableE* _mptr() const @trusted
        {
            return cast(typeof(return))(elms.ptr);
        }
    }

    static assert(Large.sizeof ==
                  Small.sizeof);

    static if (E.sizeof == 1 &&
               size_t.sizeof == 8 && // 64-bit
               Small.capacity == 23)
    {
        static assert(Large.sizeof == 24);
        static assert(Small.sizeof == 24);
    }

    union
    {
        Large _large;            // indirected storage
        Small _small;            // non-indirected storage
    }
}

import std.traits : hasMember, isDynamicArray;

/** Return an instance of `R` with capacity `capacity`. */
R withCapacityMake(R)(size_t capacity)
    if (hasMember!(R, "withCapacity"))
{
    return R.withCapacity(capacity);
}
/// ditto
R withCapacityMake(R)(size_t capacity)
    if (isDynamicArray!R)
{
    R r;
    // See http://forum.dlang.org/post/nupffaitocqjlamffuqi@forum.dlang.org
    r.reserve(capacity);
    return r;
}

pure nothrow unittest
{
    immutable capacity = 10;
    auto x = capacity.withCapacityMake!(int[]);
    assert(x.capacity >= capacity);
}

/** Return an instance of `R` of length `length`. */
R withLengthMake(R)(size_t length)
    if (hasMember!(R, "withLength"))
{
    return R.withLength(length);
}
/// ditto
R withLengthMake(R)(size_t length)
    if (isDynamicArray!R)
{
    R r;
    r.length = length;
    return r;
}

/** Return an instance of `R` containing a single element `e`. */
R withElementMake(R)(typeof(R.init[0]) e)
    if (hasMember!(R, "withElement"))
{
    return R.withElement(e);
}
/// ditto
R withElementMake(R)(typeof(R.init[0]) e)
    if (isDynamicArray!R)
{
    return [e];
}

alias UncopyableArray(E, bool useGCAllocation = false) = Array!(E, Assignment.disabled, Ordering.unsorted, useGCAllocation, size_t, "a < b");
alias CopyableArray  (E, bool useGCAllocation = false) = Array!(E, Assignment.copy, Ordering.unsorted, useGCAllocation, size_t, "a < b");

alias SortedCopyableArray    (E, bool useGCAllocation = false, alias less = "a < b") = Array!(E, Assignment.copy, Ordering.sortedValues, useGCAllocation, size_t, less);
alias SortedSetCopyableArray (E, bool useGCAllocation = false, alias less = "a < b") = Array!(E, Assignment.copy, Ordering.sortedUniqueSet, useGCAllocation, size_t, less);

alias SortedUncopyableArray    (E, bool useGCAllocation = false, alias less = "a < b") = Array!(E, Assignment.disabled, Ordering.sortedValues, useGCAllocation, size_t, less);
alias SortedSetUncopyableArray (E, bool useGCAllocation = false, alias less = "a < b") = Array!(E, Assignment.disabled, Ordering.sortedUniqueSet, useGCAllocation, size_t, less);

// string aliases
alias UncopyableString (bool useGCAllocation = false) = Array!(char,  Assignment.disabled, Ordering.unsorted, useGCAllocation, size_t, "a < b");
alias CopyableString   (bool useGCAllocation = false) = Array!(char,  Assignment.copy, Ordering.unsorted, useGCAllocation, size_t, "a < b");
alias UncopyableWString(bool useGCAllocation = false) = Array!(wchar, Assignment.disabled, Ordering.unsorted, useGCAllocation, size_t, "a < b");
alias CopyableWString  (bool useGCAllocation = false) = Array!(wchar, Assignment.copy, Ordering.unsorted, useGCAllocation, size_t, "a < b");
alias UncopyableDString(bool useGCAllocation = false) = Array!(dchar, Assignment.disabled, Ordering.unsorted, useGCAllocation, size_t, "a < b");
alias CopyableDString  (bool useGCAllocation = false) = Array!(dchar, Assignment.copy, Ordering.unsorted, useGCAllocation, size_t, "a < b");

pure unittest
{
    import std.conv : to;
    foreach (assignment; AliasSeq!(Assignment.disabled, Assignment.copy))
    {
        foreach (Ch; AliasSeq!(char, wchar, dchar))
        {
            alias Str = Array!(Ch, assignment);
            Str str_as = Str.withElement('a');
            Str str_as2 = 'a'.withElementMake!Str;
            Str str_as3 = 'a'.withElementMake!(Ch[]);
            assert(str_as == str_as2);
            assert(str_as2 == str_as3);
            str_as ~= Ch('_');
            assert(str_as[].equal("a_"));
        }
    }
}

static void tester(Ordering ordering, bool supportGC, alias less)()
{
    import std.functional : binaryFun;
    import std.range : iota, chain, repeat, only, ElementType;
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
        alias Str = Array!(Ch, assignment, ordering, supportGC, size_t, less);
        auto y = Str.withElements('a', 'b', 'c');
        static assert(is(Unqual!(ElementType!Str) == Ch));
        static assert(y.isString);
        y = Str.init;

        const(Ch)[] xs;
        {
            // immutable
            immutable x = Str.withElements('a', 'b', 'c');
            static if (!IsOrdered!ordering)
            {
                xs = x[];       // TODO should fail with DIP-1000 scope
            }
        }
    }

    foreach (Ch; AliasSeq!(char))
    {
        alias Str = Array!(Ch, assignment, ordering, supportGC, size_t, less);
        auto str = Str.withElements('a', 'b', 'c');

        static assert(str.isString);

        static if (IsOrdered!ordering)
        {
            static assert(is(Unqual!(ElementType!Str) == Ch));
        }
        else
        {
            static assert(is(ElementType!Str == Ch));
            assert(str[] == `abc`); // TODO this fails for wchar and dchar
        }
    }

    {
        alias A = Array!(int, assignment, ordering, supportGC, size_t, less);
        foreach (immutable n; [0, 1, 2, 3, 4, 5])
        {
            assert(A.withLength(n).isSmall);
        }
        assert(!(A.withLength(6).isSmall));
        assert((A.withLength(6).isLarge));
    }

    // test move construction
    {
        immutable maxLength = 1024;
        foreach (immutable n; 0 .. maxLength)
        {
            auto x = Array!(E, assignment, ordering, supportGC, size_t, less).withLength(n);

            // test resize
            static if (!IsOrdered!ordering)
            {
                assert(x.length == n);
                x.length = n + 1;
                assert(x.length == n + 1);
                x.length = n;
            }

            const ptr = x.ptr;
            immutable capacity = x.capacity;
            assert(x.length == n);

            import std.algorithm.mutation : move;
            auto y = Array!(E, assignment, ordering, supportGC, size_t, less)();
            move(x, y);

            assert(x.length == 0);
            assert(x.capacity == x.smallCapacity);

            assert(y.length == n);
            assert(y.capacity == capacity);
        }
    }

    foreach (immutable n; chain(0.only, iota(0, 10).map!(x => 2^^x)))
    {
        import std.array : array;
        import std.range : radial;

        immutable zi = cast(int)0; // zero index
        immutable ni = cast(int)n; // number index

        auto fw = iota(zi, ni); // 0, 1, 2, ..., n-1

        auto bw = fw.array.radial;

        Array!(E, assignment, ordering, supportGC, size_t, less) ss0 = bw; // reversed
        static assert(is(Unqual!(ElementType!(typeof(ss0))) == E));
        static assert(isInstanceOf!(Array, typeof(ss0)));
        assert(ss0.length == n);

        static if (IsOrdered!ordering)
        {
            if (!ss0.empty) { assert(ss0[0] == ss0[0]); } // trigger use of opindex
            assert(ss0[].equal(fw.array
                                 .sort!comp));
            assert(ss0[].isSorted!comp);
        }

        Array!(E, assignment, ordering, supportGC, size_t, less) ss1 = fw; // ordinary
        assert(ss1.length == n);

        static if (IsOrdered!ordering)
        {
            assert(ss1[].equal(fw.array
                                 .sort!comp));
            assert(ss1[].isSorted!comp);
        }

        Array!(E, assignment, ordering, supportGC, size_t, less) ss2 = fw.filter!(x => x & 1);
        assert(ss2.length == n/2);

        static if (IsOrdered!ordering)
        {
            assert(ss2[].equal(fw.filter!(x => x & 1)
                                 .array
                                 .sort!comp));
            assert(ss2[].isSorted!comp);
        }

        auto ssA = Array!(E, assignment, ordering, supportGC, size_t, less).withLength(0);
        static if (IsOrdered!ordering)
        {
            static if (less == "a < b")
            {
                alias A = Array!(E, assignment, ordering, supportGC, size_t, less);
                const A x = [1, 2, 3, 4, 5, 6];
                assert(x.front == 1);
                assert(x.back == 6);
                assert(x.lowerBound(3).equal([1, 2]));
                assert(x.upperBound(3).equal([4, 5, 6]));
                assert(x[].equal(x[])); // already sorted
            }

            foreach (i; bw)
            {
                static if (ordering == Ordering.sortedUniqueSet)
                {
                    assert(ssA.insertMany(i)[].equal([true]));
                    assert(ssA.insertMany(i)[].equal([false]));
                }
                else
                {
                    ssA.insertMany(i);
                }
            }
            assert(ssA[].equal(sort!comp(fw.array)));

            auto ssB = Array!(E, assignment, ordering, supportGC, size_t, less).withLength(0);
            static if (ordering == Ordering.sortedUniqueSet)
            {
                assert(ssB.insertMany(1, 7, 4, 9)[].equal(true.repeat(4)));
                assert(ssB.insertMany(3, 6, 8, 5, 1, 9)[].equal([true, true, true, true, false, false]));
                assert(ssB.insertMany(3, 0, 2, 10, 11, 5)[].equal([false, true, true, true, true, false]));
                assert(ssB.insertMany(0, 2, 10, 11)[].equal(false.repeat(4))); // false becuse already inserted
                assert(ssB.capacity == 16);
            }
            else
            {
                ssB.insertMany(1, 7, 4, 9);
                ssB.insertMany(3, 6, 8, 5);
                ssB.insertMany(0, 2, 10, 11);
                assert(ssB.capacity == 16);
            }

            auto ssI = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].sort!comp; // values
            immutable ssO = [12, 13]; // values not range

            assert(ssB[].equal(ssI));

            foreach (s; ssI) { assert(ssB.contains(s)); }
            foreach (s; ssO) { assert(!ssB.contains(s)); }

            ssB.compress();
            assert(ssB.capacity == 12);
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

            ssA.compress();

            // popBack
            ssA[0] = 1;
            ssA[1] = 2;
            assert(ssA[].equal([1, 2, 1]));
            assert(!ssA.empty);
            assert(ssA.front == 1);
            assert(ssA.back == 1);

            assertNotThrown(ssA.popBack());
            assert(ssA[].equal([1, 2]));
            assert(!ssA.empty);
            assert(ssA.front == 1);
            assert(ssA.back == 2);

            assertNotThrown(ssA.popBack());
            assert(ssA[].equal([1]));
            assert(!ssA.empty);
            assert(ssA.front == 1);
            assert(ssA.back == 1);

            assertNotThrown(ssA.popBack());
            assert(ssA.length == 0);
            assert(ssA.empty);
            assert(ssA.capacity != 0);

            ssA.compress();
            assert(ssA.length == 0);
            assert(ssA.empty);

            // insertAt
            ssA ~= 1;
            ssA ~= 2;
            ssA ~= 3;
            ssA ~= 4;
            ssA ~= 5;
            ssA ~= 6;
            ssA ~= 7;
            ssA ~= 8;
            assert(ssA[].equal([1, 2, 3, 4, 5, 6, 7, 8]));
            ssA.insertAtIndex(3, 100, 101);
            assert(ssA[].equal([1, 2, 3, 100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront());
            assert(ssA[].equal([2, 3, 100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront());
            assert(ssA[].equal([3, 100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront());
            assert(ssA[].equal([100, 101, 4, 5, 6, 7, 8]));
            assertNotThrown(ssA.linearPopFront());
            assertNotThrown(ssA.linearPopFront());
            assertNotThrown(ssA.linearPopFront());
            assertNotThrown(ssA.linearPopFront());
            assertNotThrown(ssA.linearPopFront());
            assertNotThrown(ssA.linearPopFront());
            assertNotThrown(ssA.linearPopFront());
            assert(ssA.empty);
            ssA.compress();

            // popAtIndex
            ssA ~= 1;
            ssA ~= 2;
            ssA ~= 3;
            ssA ~= 4;
            ssA ~= 5;
            assertNotThrown(ssA.popAtIndex(2));
            assert(ssA[].equal([1, 2, 4, 5]));

            // pushBack and assignment from slice
            auto ssB = Array!(E, assignment, ordering, supportGC, size_t, less).withLength(0);
            ssB.pushBack([1, 2, 3, 4, 5]);
            ssB.pushBack([6, 7]);
            assert(ssB[].equal([1, 2, 3, 4, 5, 6, 7]));
            assert(ssB.backPop() == 7);
            assert(ssB.backPop() == 6);
            assert(ssB.backPop() == 5);
            assert(ssB.backPop() == 4);
            assert(ssB.backPop() == 3);
            assert(ssB.backPop() == 2);
            assert(ssB.backPop() == 1);
            assert(ssB.empty);

            // pushBack(Array)
            {
                immutable s = [1, 2, 3];
                Array!(E, assignment, ordering, supportGC, size_t, less) s1 = s;
                Array!(E, assignment, ordering, supportGC, size_t, less) s2 = s1[];
                assert(s1[].equal(s));
                s1 ~= s1;
                assert(s1[].equal(chain(s, s)));
                s1 ~= s2;
                assert(s1[].equal(chain(s, s, s)));
            }

            immutable ss_ = Array!(E, assignment, ordering, supportGC, size_t, less)(null);
            assert(ss_.empty);

            auto ssC = Array!(E, assignment, ordering, supportGC, size_t, less).withLength(0);
            immutable(int)[] i5 = [1, 2, 3, 4, 5];
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
    alias E = ubyte;
    alias A = Array!(E, Assignment.disabled, Ordering.unsorted, false, size_t, "a < b");
    A a;
    immutable n = ubyte.max;
    size_t i = 0;
    foreach (ubyte e; 0 .. n)
    {
        a ~= e;
        // assert(a.back == e.to!E);
        assert(a.length == i + 1);
        ++i;
    }
    const b = a.dup;
    static assert(is(typeof(a) == Unqual!(typeof(b))));
    assert(b.length == a.length);
    assert(a !is b);
    assert(a == b);
}

/// disabled copying
@safe nothrow unittest
{
    alias E = string;

    alias A = Array!(E, Assignment.disabled, Ordering.unsorted, false, size_t, "a < b");
    A a;
    immutable n = 100_000;
    size_t i = 0;
    foreach (const ref e; 0 .. n)
    {
        a ~= e.to!E;
        // assert(a.back == e.to!E);
        assert(a.length == i + 1);
        ++i;
    }
    const b = a.dup;
    static assert(is(typeof(a) == Unqual!(typeof(b))));
    assert(b.length == a.length);
    assert(a !is b);
    assert(a == b);
}

/// disabled copying
nothrow unittest
{
    alias E = string;
    alias A = Array!(E, Assignment.disabled, Ordering.unsorted, false, size_t, "a < b");
    A a;
    immutable n = 100_000;
    size_t i = 0;
    foreach (const ref e; 0 .. n)
    {
        a ~= e.to!E;
        assert(a.length == i + 1);
        ++i;
    }
    const b = a.dup;
    static assert(is(typeof(a) == Unqual!(typeof(b))));
    assert(a[] == b[]);
}

/// disabled copying
nothrow unittest
{
    import std.traits : isRvalueAssignable, isLvalueAssignable;

    alias E = string;

    alias A = Array!E;
    static assert(!isCopyable!(A));

    alias CA = CopyableArray!E;
    static assert(isCopyable!(CA));

    static assert(isRvalueAssignable!(A));
    static assert(isLvalueAssignable!(A));

    // import std.range.primitives : hasSlicing;
    // TODO make this evaluate to `true`
    // static assert(hasSlicing!A);

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
        immutable n = 100;
        foreach (_; 0 .. n)
        {
            auto e = E.init;
            x ~= move(e);
            y ~= E.init;
        }
        foreach (_; 0 .. n)
        {
            assert(x.backPop() == E.init);
            assert(y.backPop() == E.init);
        }
        assert(x.length == 1);
        assert(y.length == 2);

        import std.algorithm : swap;
        swap(x, y);
        assert(x.length == 2);
        assert(y.length == 1);

        swap(x[0], y[0]);
    }

}

/// assert same behaviour of `dup` as for builtin arrays
@safe pure nothrow unittest
{
    struct Vec { int x, y; }
    class Db { int* _ptr; }
    struct Node { int x; class Db; }
    // struct Node1 { const(int) x; class Db; }
    foreach (E; AliasSeq!(int, const(int), Vec, Node// , Node1
                 ))
    {
        alias DA = E[];         // builtin D array/slice
        immutable DA da = [E.init]; // construct from array
        auto daCopy = da.dup;   // duplicate
        daCopy[] = E.init;   // opSliceAssign

        alias CA = Array!E;         // container array
        immutable ca = CA.withElement(E.init);

        auto caCopy = ca.dup;
        // TODO
        // pragma(msg, typeof(caCopy));
        // pragma(msg, E);
        // caCopy ~= const(E).init;

        // should have same element type
        static assert(is(typeof(caCopy[0]) ==
                         typeof(daCopy[0])));

    }
}

/// array as AA key type
@safe pure nothrow unittest
{
    struct E { int x, y; }
    foreach (A; AliasSeq!(Array!E,
                          CopyableArray!E))
    {
        int[A] x;
        immutable n = 100;
        foreach (immutable i; 0 .. n)
        {
            assert(x.length == i);
            assert(A.withElement(E(i, 2*i)) !in x);
            x[A.withElement(E(i, 2*i))] = 42;
            assert(x.length == i + 1);
            auto a = A.withElement(E(i, 2*i));
            static if (isCopyable!A)
            {
                // TODO why do these fail when `A` is uncopyable?
                assert(a in x);
                assert(A.withElement(E(i, 2*i)) in x);
                assert(x[A.withElement(E(i, 2*i))] == 42);
            }
        }
    }
}

/// init and append to empty array as AA value type
@safe pure nothrow unittest
{
    alias Key = string;
    alias A = Array!int;

    A[Key] x;

    assert("a" !in x);

    x["a"] = A.init;            // if this init is removed..
    x["a"] ~= 42;               // ..then this fails

    assert(x["a"] == A.withElement(42));
}

/// compress
@safe pure nothrow @nogc unittest
{
    alias A = Array!string;
    A a;

    a.compress();

    a ~= "a";
    a ~= "b";
    a ~= "c";

    assert(a.length == 3);
    assert(a.capacity == 4);

    a.compress();

    assert(a.capacity == a.length);
}

///
@safe pure nothrow @nogc unittest
{
    alias Key = UncopyableArray!char;
    alias Value = UncopyableArray!int;
    struct E
    {
        Key key;
        Value value;
        E dup() @safe pure nothrow @nogc
        {
            return E(key.dup, value.dup);
        }
    }
    E e;
    e.key = Key.withElement('a');
    e.value = Value.withElement(42);

    auto f = e.dup;
    assert(e == f);

    e.key = Key.withElement('b');
    assert(e != f);

    e.key = Key.withElement('a');
    assert(e == f);

    e.value = Value.withElement(43);
    assert(e != f);

    e.value = Value.withElement(42);
    assert(e == f);

}

/// append to empty to array as AA value type
pure unittest
{
    import std.exception: assertThrown;
    import core.exception : RangeError;
    alias Key = string;
    alias A = Array!int;
    A[Key] x;
    // assertThrown!RangeError({ x["a"] ~= 42; }); // TODO make this work
}

/// map array of uncopyable
unittest
{
    import std.range : isInputRange;
    import std.array : array;
    import std.algorithm.iteration : map;
    alias A = UncopyableArray!int;
    auto y = A.init[].map!(_ => _^^2).array;
    A z = y.dup;                // check that dup returns same type
}

/// collection
/*@safe*/ pure nothrow @nogc unittest // TODO make @safe when collect has been made safe
{
    import std.range : iota, isOutputRange;
    import algorithm_ex : collect;

    alias E = int;
    alias A = Array!E;

    immutable n = 100;
    static assert(isOutputRange!(A, E));

    assert((0.iota(n).collect!A)[].equal(0.iota(n)));
}

/// map array of uncopyable
pure unittest
{
    foreach (AT; AliasSeq!(SortedUncopyableArray, SortedSetUncopyableArray))
    {
        alias A = AT!int;
        A a;
        A b = a.dup;
    }
}
