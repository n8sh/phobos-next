module nxt.open_hashmap;

// version = showEntries;

import core.internal.hash : hashOf;
import nxt.nullable_traits : isNullable;
import nxt.pure_mallocator : Mallocator = PureMallocator; // TODO: merge into `std.experimental.allocator.mallocator`

enum Flag
{
    borrowChecked,
    useSmallLinearSearch,
    usePrimeCapacity,
}
import std.typecons : BitFlags;
alias Flags = BitFlags!Flag;    ///< Use as Flags flags param to `OpenHashMap`

@safe:

/** Hash table/map with in-place open-addressing, storing `keys` of type `K` and
 * values of type `V`.
 *
 * Keys are immutable except for when they are `class`es in which case they are
 * head-const (through bin reinterpretation to `KeyValueType`), This can be
 * overridden by setting `keyEqualPred` to, for instance, `a == b` for `class`
 * keys.
 *
 * Uses quadratic probing (using triangular numbers) unless `usePrimeCapacity`
 * in which case a simpler probing is used.
 *
 * Deletion/Removal of elements is lazy via the bitmap `_holesPtr` or through
 * assignment of of reserved value of `KeyType` when `KeyType` has hole-support
 * via trait `isHoleable`.
 *
 * Element iteration via
 * - either `byKey`, `byValue` or `byKeyValue` over `OpenHashMap` and
 * - `byElement` over `OpenHashset`
 * respects taking the container argument either as an l-value or r-value using
 * detected using `auto ref`-qualified parameter introspected using `(__traits(isRef, y))`.
 * In the r-value case no reference counting is needed.
 * In the l-value case setting `borrowChecked` to `true` adds run-time
 * support for dynamic Rust-style ownership and borrowing between the range and the container.
 *
 * Params:
 *      K = key type
 *      V = value type
 *      hasher = hash function or std.digest Hash
 *      Allocator = memory allocator for bin array
 *      borrowChecked = only activate when it's certain that this won't be moved via std.algorithm.mutation.move()
 *      useSmallLinearSearch = Use linear search instead probing when `_store` is smaller than `linearSearchMaxSize`
 *      usePrimeCapacity = Use prime numbers as capacity of hash table enabling better performance of simpler hash-functions
 *
 * See_Also: https://www.sebastiansylvan.com/post/robin-hood-hashing-should-be-your-default-hash-table-implementation/
 * See_Also: https://arxiv.org/abs/1605.04031
 * See_Also: https://github.com/Tessil/robin-map
 * See_Also: https://github.com/martinus/robin-hood-hashing
 * See_Also: https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/
 * See_Also: https://en.wikipedia.org/wiki/Lazy_deletion
 * See_Also: https://forum.dlang.org/post/ejqhcsvdyyqtntkgzgae@forum.dlang.org
 * See_Also: https://gankro.github.io/blah/hashbrown-insert/
 *
 * TODO: Disable pragma(inline, true) and rebenchmark
 *
 * TODO: tests fails when `useSmallLinearSearch` is set to `false`
 *
 * TODO: Use set of `Flag`s (defined here) as template params
 *
 * Robin-hood case introspect key type for storage of parts of hash alongside
 * nullValue and holeValue. Typically for address-types that doesn't need
 * scanning. Example is `Address` for implementation of GC. This is a D showcase
 * for code that is difficult to write in C++.
 *
 * TODO: group `nxt.probing` functions in `Prober` struct given as type template
 * param to `OpenHashMap`
 *
 * TODO: Make load factor dependent on current capacity or length and perhaps
 * also type and hash-function to get memory efficiency when it matters. Similar
 * to what is recommended in https://ticki.github.io/blog/horrible/.
 *
 * TODO: For copyable types replace `auto ref` with logic that only passes by `ref`
 * when it's faster to do so. See_Also: https://github.com/dlang/dmd/pull/11000
 *
 * TODO: remove use of `static if (__traits(isCopyable, ...))` in cases where compiler can handle more moves
 *
 * TODO: use mmap allocator when `_store.sizeof` is larger than at least 8 pages
 *
 * TODO: use `StoreK` in store and cast between it and `KeyType`
 *
 * TODO: allocate _holesPtr array together with _store to reduce size of
 * `OpenHashMap` to 3 words when element type doesn't support it
 *
 * TODO: fix bug in `growInPlaceWithCapacity` and benchmark
 *
 * TODO: modify existing unittest for `struct Rel { const string name; }`
 *
 * TODO: use allocator.dispose() instead of allocator.deallocate() as in
 * https://github.com/dlang-community/containers
 *
 * TODO: if hash-function is cast(size_t)(classInstance) always use prime length
 * and shift pointer before hash based on alignof (might not be needed when
 * module prime) to maximize memory locality when adding successively allocated
 * pointers
 *
 * TODO: add extractElement that moves it out similar to
 * http://en.cppreference.com/w/cpp/container/unordered_set/extract
 *
 * TODO: add merge or union algorithm here or into container_algorithm.d. See
 * also: http://en.cppreference.com/w/cpp/container/unordered_set/merge. this
 * algorithm moves elements from source if they are not already in `this`
 *
 * TODO: Robin-Hood-hashing
 *
 * TODO: enable `borrowChecked` unconditionally in version(debug) if and when
 * `opMove` is implemented, in which case opMove() should assert false if this
 * is borrowed. See: https://github.com/dlang/DIPs/pull/109
 *
 * TODO: keep only predicates with ref arguments (`const scope auto ref`) when
 * LDC can optimize those as fast as value passing. add LDC issue for this
 *
 * TODO: save one word by making `_store.length` be inferred by
 * `primeConstants[_primeIndex]` if this is not too costly
 *
 * TODO: only add one extra element to capacity when `assumeNonFullHaystack` is `true`
 */
struct OpenHashMap(K, V = void,
                   alias hasher = hashOf,
                   string keyEqualPred = defaultKeyEqualPredOf!(K),
                   alias Allocator = Mallocator.instance,
                   bool borrowChecked = false,
                   bool useSmallLinearSearch = true,
                   bool usePrimeCapacity = false)
if (isNullable!K /*&& !hasAliasing!K */)
{
    // pragma(msg, K.stringof, " => ", V.stringof);
    import core.exception : onOutOfMemoryError;
    import core.internal.traits : hasElaborateDestructor, Unqual;
    import core.lifetime : move;
    import std.traits : hasIndirections, hasFunctionAttributes;
    import std.typecons : Nullable;

    import nxt.nullable_traits : defaultNullKeyConstantOf, isNull, nullify;
    import nxt.container_traits : mustAddGCRange;
    import nxt.qcmeman : gc_addRange, gc_removeRange;

    static if (usePrimeCapacity)
        import nxt.prime_modulo : PrimeIndex, ceilingPrime, moduloPrimeIndex;
    else
    {
        import std.math : nextPow2;
        import nxt.probing : triangularProbeFromIndex, triangularProbeFromIndexIncludingHoles, triangularProbeCountFromIndex;
        /// Setting this `true` doesn't give measurable speedups so set it to `false` for now.
        enum bool assumeNonFullHaystack = false;
    }

    static if (is(typeof(keyEqualPred) : string))
    {
        import std.functional : binaryFun;
        alias keyEqualPredFn = binaryFun!keyEqualPred;
    }
    else
        alias keyEqualPredFn = keyEqualPred;

    private enum isSlice(T) = is(T : const(E)[], E);

    static if ((is(K == class)) &&
               keyEqualPred == `a is b`) // TODO: use better predicate compare?
        alias StoreK = void*;
    else
    {
        import std.traits : isPointer;
        static if (isPointer!K &&
                   // TODO: use better predicate compare?
                   (keyEqualPred == `a == b` ||
                    keyEqualPred == `a is b`))
            alias StoreK = void*;
        else
            alias StoreK = K;
    }

    enum isBorrowChecked = borrowChecked;

    /** In the hash map case, `V` is non-void, and a value is stored alongside
     * the key of type `K`.
     */
    enum hasValue = !is(V == void);

    /** Is `true` iff `K` is an address, in which case holes are represented by
     * a specific value `holeKeyConstant`.
     */
    enum hasAddressLikeKey = (isAddress!K ||
                              isSlice!K);

    /** Stores less than or equal to this size will be searched using linear search. */
    private enum linearSearchMaxSize = 64; // one cache-line for now

    static if (hasAddressLikeKey)
    {
        enum hasHoleableKey = true;
        enum holeKeyOffset = 0x1; // TODO: is this a good value? Or is 0xffff_ffff_ffff_ffff better?
        @trusted enum holeKeyAddress = cast(void*)holeKeyOffset;

        /**
         * See_Also: https://forum.dlang.org/post/p7726n$2apd$1@digitalmars.com
         * TODO: test if ulong.max gives better performance
         */
        static K holeKeyConstant() @trusted pure nothrow @nogc
        {
            version(D_Coverage) {} else pragma(inline, true);
            // TODO: note that cast(size_t*) will give address 0x8 instead of 0x1
            static if (isSlice!K)
            {
                alias E = typeof(K.init[0])*; // array element type
                auto ptr = cast(E)((cast(void*)null) + holeKeyOffset); // indicates a lazily deleted key
                return ptr[0 .. 0];
            }
            else
            {
                return cast(K)((cast(void*)null) + holeKeyOffset); // indicates a lazily deleted key
            }
        }

        static bool isHoleKeyConstant(const scope K key) @trusted pure nothrow @nogc
        {
            version(D_Coverage) {} else pragma(inline, true);
            static if (isSlice!K) // for slices
                // suffice to compare pointer part
                return (key.ptr is holeKeyAddress);
            else
                return (cast(const(void)*)key is holeKeyAddress);
        }

        /** TODO: make these work
         */
        // enum K holeKey_1 = cast(K)((cast(size_t*)null));
        // static immutable K holeKey_2 = cast(K)((cast(size_t*)null));
    }
    else static if (isHoleable!K)
    {
        enum hasHoleableKey = true;
        static K holeKeyConstant() @safe pure nothrow @nogc
        {
            version(D_Coverage) {} else pragma(inline, true);
            return K.holeValue;
        }
        static bool isHoleKeyConstant(const scope K key) @safe pure nothrow @nogc
        {
            version(D_Coverage) {} else pragma(inline, true);
            static if (__traits(hasMember, K, "isHole"))
                // typically faster by asserting value of member of aggregate `K`
                return key.isHole;
            else
                return key is K.holeValue;
        }
    }
    else static if (__traits(hasMember, K, "nullifier"))
    {
        alias Nullifier = typeof(K.init.nullifier);
        // TODO: pragma(msg, K, " has nullifier ", Nullifier);
        static if (isHoleable!Nullifier)
        {
            // TODO: pragma(msg, K, " has holeable nullifier ", Nullifier);
            enum hasHoleableKey = true;
            static K holeKeyConstant() @trusted pure nothrow @nogc
            {
                K k;
                k.nullifier = Nullifier.holeValue;
                return k;
            }
            static bool isHoleKeyConstant(const scope K key) @trusted pure nothrow @nogc
            {
                return key.nullfier == Nullifier.holeValue;
            }
        }
        else
        {
            enum hasHoleableKey = false;
            // pragma(msg, "Need explicit hole bitset for non-address-like key: ", K);
            import core.bitop : bts, bt, btr;
            import nxt.array_help : makeUninitializedBitArray, makeZeroedBitArray, makeReallocatedBitArrayZeroPadded;
        }
    }
    else
    {
        enum hasHoleableKey = false;
        // pragma(msg, "Need explicit hole bitset for non-address-like key: ", K);
        import core.bitop : bts, bt, btr;
        import nxt.array_help : makeUninitializedBitArray, makeZeroedBitArray, makeReallocatedBitArrayZeroPadded;
    }

    /// Element type.
    static if (hasValue)
    {
        /** Map insertion status.
         */
        enum InsertionStatus
        {
            added,              ///< Element was added.
            modified,           ///< Value of element was changed (map only).
            unmodified          ///< Element was left unchanged.
        }

        /// Mutable element reference with mutable constant key and value.
        struct T
        {
            K key;
            V value;
        }

        /// Get key part of element.
        static auto ref inout(K) keyOf(SomeElement)(auto ref return scope inout(SomeElement) element)
        {
            version(D_Coverage) {} else pragma(inline, true);
            return element.key;
        }

        /// Get value part of element.
        static auto ref inout(V) valueOf()(auto ref return scope inout(T) element)
        {
            version(D_Coverage) {} else pragma(inline, true);
            return element.value;
        }

        /** Type of key stored. */
        public alias KeyType = K;

        /** Type of value stored. */
        public alias ValueType = V;

        enum nullKeyElement = T(defaultNullKeyConstantOf!K, V.init);

        /// Key-value element reference with head-const for `class` keys and mutable value.
        static private struct KeyValueType
        {
            static if (isAddress!K) // for reference types
            {
                K _key;          // no const because
                /** Key access is head-const. */
                inout(K) key() @property inout @safe pure nothrow @nogc
                {
                    return _key;
                }
            }
            else
                const K key;
            V value;
        }

        /// Get key part.
        static auto ref inout(K) keyOf()(auto ref return scope inout(KeyValueType) element) @trusted
        {
            version(D_Coverage) {} else pragma(inline, true);
            return cast(typeof(return))element.key; // needed for case: `inout(const(K)) => inout(K)`
        }
    }
    else                        // HashSet
    {
        /** Set insertion status.
         */
        enum InsertionStatus
        {
            added,              ///< Element was added.
            unmodified          ///< Element was left unchanged.
        }

        alias T = K;            // short name for element type

        /// Get key part of element.
        static auto ref inout(SomeElement) keyOf(SomeElement)(auto ref return inout(SomeElement) element)
        {
            version(D_Coverage) {} else pragma(inline, true);
            return element;
        }

        enum nullKeyElement = defaultNullKeyConstantOf!K;
    }

    /** Is `true` if an instance of `SomeKey` that can be implictly cast to `K`.
     *
     * For instance `const(char)[]` can be `@trusted`ly cast to `string` in a
     * temporary scope.
     */
    template isScopedKeyType(SomeKey)
    {
        static if (is(SomeKey == class))
            enum isScopedKeyType = (is(const(SomeKey) : const(K)));
        else
            enum isScopedKeyType = (is(K : SomeKey) || // `K is` implicitly convertible from `SomeKey`
                                    is(SomeKey : U[], U) && // is array
                                    is(typeof(K(SomeKey.init))));
    }

    alias ElementType = T;

    /** Make with room for storing at least `minimumCapacity` number of elements.
     *
     * See_Also:
     * https://forum.dlang.org/post/nyngzsaeqxzzuumivtze@forum.dlang.org
     */
    static typeof(this) withCapacity()(size_t minimumCapacity) // template-lazy
    {
        version(showEntries) dbg(__FUNCTION__, " minimumCapacity:", minimumCapacity);
        static if (usePrimeCapacity)
        {
            PrimeIndex primeIndex;
            immutable initialCapacity = ceilingPrime(minimumCapacity + 1, primeIndex);
            assert(minimumCapacity < initialCapacity); // we need at least one vacancy
            return typeof(return)(makeDefaultInitializedStoreOfCapacity(initialCapacity), primeIndex, 0);
        }
        else
        {
            immutable initialCapacity = nextPow2(minimumCapacity);
            assert(minimumCapacity < initialCapacity); // we need at least one vacancy
            return typeof(return)(makeDefaultInitializedStoreOfCapacity(initialCapacity), 0);
        }
    }

    /** Make default-initialized store with `capacity` number of slots.
     */
    static private T[] makeDefaultInitializedStoreOfCapacity()(in size_t capacity) @trusted pure nothrow @nogc // template-lazy
    {
        static if (usePrimeCapacity)
        {
            // TODO: check that capacity is prime?
        }
        else
        {
            debug import std.math : isPowerOf2;
            debug assert(isPowerOf2(capacity)); // quadratic probing needs power of two capacity (`_store.length`)
        }
        version(showEntries) dbg(__FUNCTION__, " minimumCapacity:",
                                 minimumCapacity,
                                 " capacity:", capacity);

        // TODO: cannot use makeArray here because it cannot handle uncopyable types
        // import std.experimental.allocator : makeArray;
        // auto store = Allocator.makeArray!T(capacity, nullKeyElement);

        import nxt.bit_traits : isAllZeroBits;

        immutable byteCount = T.sizeof*capacity;

        static if (hasAddressLikeKey ||
                   (__traits(isZeroInit, K)  &&
                    __traits(hasMember, K, "nullifier")) ||
                   // TODO: add check for __traits(isZeroInit, K) and member `K.nullValue` == `K.init`
                   (__traits(hasMember, K, `nullValue`) && // if key has a null value
                    __traits(compiles, { enum _ = isAllZeroBits!(K, K.nullValue); }) && // prevent strange error given when `K` is `knet.data.Data`
                    isAllZeroBits!(K, K.nullValue))) // check that it's zero bits only
        {
            // pragma(msg, "zero-allocate:", "K:", K, " V:", V);
            // TODO: use std.experimental.allocator.makeArray instead of this which handles clever checking for isZeroInit
            import nxt.container_traits : makeInitZeroArray;
            auto store = makeInitZeroArray!(T, Allocator)(capacity);
            if (store.ptr is null && capacity >= 1)
            {
                onOutOfMemoryError();
            }
        }
        else                    // when default null key is not represented by zeros
        {
            // pragma(msg, "emplace:", "K:", K, " V:", V);
            auto store = cast(T[])Allocator.allocate(byteCount);
            if (store.ptr is null && byteCount >= 1)
            {
                onOutOfMemoryError();
            }
            foreach (ref bin; store)
            {
                import core.lifetime : emplace;
                enum hasNullValueKey = __traits(hasMember, K, `nullValue`);
                static if (hasNullValueKey &&
                           !is(typeof(emplace(&keyOf(bin), K.nullValue)))) // __traits(compiles) fails here when building knet
                    pragma(msg, __FILE__, ":", __LINE__, ":warning: emplace fails for null-Value key type ", K);

                // initialize key
                static if (hasNullValueKey &&
                           is(typeof(emplace(&keyOf(bin), K.nullValue))))
                    emplace(&keyOf(bin), K.nullValue); // initialize in-place with explicit `K.nullValue`
                else
                {
                    emplace(&keyOf(bin)); // initialize in-place with default value
                    keyOf(bin).nullify(); // moveEmplace doesn't init source of type `Nullable`
                }

                // initialize value
                static if (hasValue)
                {
                    static if (hasElaborateDestructor!V)
                        emplace(&valueOf(bin)); // initialize in-place
                    else static if (mustAddGCRange!V)
                        valueOf(bin) = V.init;
                    else
                    {
                        // ok for this case to have uninitialized value part
                    }
                }
            }
        }

        static if (mustAddGCRange!T)
            gc_addRange(store.ptr, byteCount);

        return store;
    }

    static private T[] allocateUninitializedStore()(size_t capacity) @trusted pure nothrow @nogc // template-lazy
    {
        version(showEntries) dbg(__FUNCTION__, " newCapacity:", capacity);
        immutable byteCount = T.sizeof*capacity;
        auto store = cast(typeof(return))Allocator.allocate(byteCount);
        static if (mustAddGCRange!T)
            gc_addRange(store.ptr, byteCount);
        if (store.ptr is null &&
            byteCount >= 1)
            onOutOfMemoryError();
        return store;
    }

    import std.range.primitives : StdElementType = ElementType;
    import std.traits : isIterable, isAssignable;

    /** Make with the element `element`. */
    this(T element)
    {
        static if (usePrimeCapacity)
        {
            _primeIndex = PrimeIndex.init;
            _store = makeDefaultInitializedStoreOfCapacity(ceilingPrime(1 + 1, _primeIndex));
        }
        else
            _store = makeDefaultInitializedStoreOfCapacity(nextPow2(1));
        _count = 0;
        static if (__traits(isCopyable, T))
            insertWithoutGrowthNoStatus(element);
        else
            insertWithoutGrowthNoStatus(move(element));
    }

    static if (hasHoleableKey)
    {
        private this(T[] store, size_t count)
        {
            _store = store;
            _count = count;
        }
    }
    else
    {
        private this(T[] store, size_t count, size_t* holesPtr = null)
        {
            _store = store;
            _count = count;
            _holesPtr = holesPtr;
        }
    }

    /** Make with the elements `elements`. */
    static typeof(this) withElements(R)(R elements)
    if (isIterable!R &&
        isAssignable!(T, StdElementType!R))
    {
        version(showEntries) dbg(__FUNCTION__, " length:", elements.length);
        import std.range.primitives : hasLength;
        static if (hasLength!R)
        {
            typeof(this) that = withCapacity(elements.length);
            foreach (element; elements)
                that.insertWithoutGrowthNoStatus(element);
        }
        else
        {
            typeof(this) that;
            foreach (ref element; elements)
                that.insert(element);
        }
        return that;
    }

    /// Destruct.
    ~this() @nogc
    {
        release();
    }

    /// No copying.
    @disable this(this);

    /// Returns: a shallow duplicate of `this`.
    typeof(this) dup()() const @trusted // template-lazy
    {
        version(showEntries) dbg(__FUNCTION__, " length:", length);

        T[] storeCopy = allocateUninitializedStore(_store.length); // unsafe

        foreach (immutable index, ref bin; _store)
            if (isOccupiedAtIndex(index)) // normal case
            {
                static if (hasValue) // map
                {
                    duplicateEmplace(bin.key, storeCopy[index].key);
                    duplicateEmplace(bin.value, storeCopy[index].value);
                }
                else            // set
                    duplicateEmplace(bin, storeCopy[index]);
            }
            else
            {
                import core.lifetime : emplace;
                emplace(&storeCopy[index]); // TODO: only emplace key and not value
                keyOf(storeCopy[index]).nullify();
            }

        static if (!hasHoleableKey)
            if (_holesPtr)
            {
                immutable wordCount = holesWordCount(_store.length);

                auto holesPtrCopy = makeUninitializedBitArray!Allocator(_store.length);
                holesPtrCopy[0 .. wordCount] = _holesPtr[0 .. wordCount]; // TODO: use memcpy instead?

                return typeof(return)(storeCopy, _count, holesPtrCopy);
            }

        return typeof(return)(storeCopy, _count);
    }

    /// Equality.
    bool opEquals()(const scope auto ref typeof(this) rhs) const
    {
        if (_count != rhs._count)
            return false;       // quick discardal

        foreach (immutable index, const ref bin; _store)
            if (isOccupiedAtIndex(index))
            {
                static if (hasValue)
                {
                    auto valuePtr = bin.key in rhs;
                    if (!valuePtr)
                        return false;
                    // TODO: make != a parameter that can also be typically !is. TODO: ask forum about this
                    if ((*valuePtr) != bin.value)
                        return false;
                }
                else
                    if (!rhs.contains(bin))
                        return false;
            }

        return true;
    }

    static if (true)
    {
    private:

        static if (!hasHoleableKey)
        {
            void deallocateHoles() @trusted
            {
                if (_holesPtr)
                {
                    static if (__traits(hasMember, Allocator, "deallocatePtr"))
                        Allocator.deallocatePtr(_holesPtr);
                    else
                        Allocator.deallocate(_holesPtr[0 .. holesWordCount(_store.length)]);
                }
            }

            static size_t* reallocateHoles(size_t[] holes, size_t byteCount) @trusted
            {
                auto rawHoles = cast(void[])holes;
                const ok = Allocator.reallocate(rawHoles, byteCount);
                assert(ok, "couldn't reallocate holes");
                return cast(typeof(return))rawHoles.ptr;
            }

            void clearHoles()
            {
                deallocateHoles();
                _holesPtr = null;
            }

            enum wordBytes = size_t.sizeof;
            enum wordBits = 8*wordBytes;

            /** Returns: number of words (`size_t`) needed to represent
             * `binCount` holes.
             */
            static size_t holesWordCount(size_t binCount)
            {
                pragma(inline, true);
                return (binCount / wordBits +
                        (binCount % wordBits ? 1 : 0));
            }

            static size_t binBlockBytes(size_t binCount)
            {
                pragma(inline, true);
                return wordBytes*holesWordCount(binCount);
            }

            void untagHoleAtIndex(size_t index) @trusted
            {
                pragma(inline, true);
                version(unittest) assert(index < _store.length);
                if (_holesPtr !is null)
                    btr(_holesPtr, index);
            }

            static bool hasHoleAtPtrIndex(const scope size_t* holesPtr, size_t index) @trusted
            {
                pragma(inline, true);
                return (holesPtr &&
                        bt(holesPtr, index) != 0);
            }
        }

        void tagHoleAtIndex(size_t index) @trusted
        {
            pragma(inline, true);
            version(unittest) assert(index < _store.length);
            static if (hasHoleableKey)
                keyOf(_store[index]) = holeKeyConstant;
            else
            {
                if (_holesPtr is null) // lazy allocation
                    _holesPtr = makeZeroedBitArray!Allocator(_store.length);
                bts(_holesPtr, index);
            }
        }
    }

    static if (borrowChecked)
        static immutable borrowedErrorMessage = "cannot mutate this when it's borrowed";

    /// Empty.
    void clear()()              // template-lazy
    {
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        release();
        _store = typeof(_store).init;
        static if (usePrimeCapacity)
            _primeIndex = 0;
        static if (!hasHoleableKey)
            _holesPtr = null;
        _count = 0;
    }

    /// Release internal allocations.
    private void release() scope
    {
        releaseBinElements();
        releaseStoreAndHolesSlices();
    }

    /// Release bin elements.
    private void releaseBinElements() scope
    {
        foreach (ref bin; _store)
        {
            static if (hasElaborateDestructor!T)
                .destroy(bin);
            else static if (mustAddGCRange!T)
                bin = T.init;
        }
    }

    /// Release bin slice.
    private void releaseStoreAndHolesSlices() scope
    {
        releaseStoreSlice(_store);
        static if (!hasHoleableKey)
            deallocateHoles();
    }

    static private void releaseStoreSlice(T[] store) @trusted
    {
        version(showEntries) dbg(__FUNCTION__, " store.ptr:", store.ptr, " store.length", store.length);
        if (store.ptr is null) { return; } // `gc_removeRange` fails for null input
        static if (mustAddGCRange!T)
            gc_removeRange(store.ptr); // `gc_removeRange` fails for null input
        static if (__traits(hasMember, Allocator, "deallocatePtr"))
            Allocator.deallocatePtr(store.ptr);
        else
            Allocator.deallocate(store);
    }

    private auto adjustKeyType(SomeKey)(const return scope SomeKey key) const scope @trusted
    {
        pragma(inline, true);            // must be inlined
        static if (is(SomeKey : U[], U)) // is array (slice)
            /* because return value is used only temporarily it's ok to cast to
             * `immutable` to prevent GC-allocations in types such as
             * `sso_string.SSOString` */
            return cast(immutable(typeof(key[0]))[])key;
        else
            return key;
    }

    /** Check if `element` is stored.
     *
     * Parameter `key` may be non-immutable, for instance const(char)[]
     * eventhough key type `K` is `string`.
     *
     * Returns: `true` if element is present, `false` otherwise.
     */
    bool contains(SomeKey)(const scope SomeKey key) const scope @trusted // template-lazy, `auto ref` here makes things slow
    if (isScopedKeyType!(typeof(key)))
    {
        // pragma(msg, SomeKey.stringof ~ " " ~ K.stringof, " ", is(K : SomeKey), " ", is(SomeKey : K));
        // debug static assert(isScopedKeyType!(typeof(key)), SomeKey.stringof ~ " " ~ K.stringof);
        version(LDC) pragma(inline, true);

        assert(!key.isNull);
        static if (hasHoleableKey) { assert(!isHoleKeyConstant(cast(const(K))adjustKeyType(key))); }

        static if (useSmallLinearSearch)
        {
            if (_store.length * T.sizeof <= linearSearchMaxSize)
                return containsUsingLinearSearch(key);
        }

        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(const(K))adjustKeyType(key)); // cast scoped `key` is @trusted

        version(none)
        static if (SomeKey.stringof == "SSOString" ||
                   is(SomeKey == const(char)[]))
        {
            dbg(SomeKey.stringof, " key:", key,
                " store length:", _store.length,
                " hitIndex:", hitIndex,
                " isOcc:", isOccupiedAtIndex(hitIndex),
                " store:", _store);
        }

        return (hitIndex != _store.length &&
                isOccupiedAtIndex(hitIndex));
    }

    /** Check if `element` is stored.
     *
     * Uses linear search instead of hashing plus probing and may be faster for
     * for small tables with complicated hash functions.
     *
     * Parameter `key` may be non-immutable, for instance const(char)[]
     * eventhough key type `K` is `string`.
     *
     * Returns: `true` if element is present, `false` otherwise.
     */
    bool containsUsingLinearSearch(SomeKey)(const scope SomeKey key) const scope @trusted // template-lazy, `auto ref` here makes things slow
    if (isScopedKeyType!(typeof(key)))
    {
        assert(!key.isNull);
        static if (hasHoleableKey) { assert(!isHoleKeyConstant(cast(const(K))adjustKeyType(key))); }

        static if (isInstanceOf!(Nullable, SomeKey))
        {
            import std.algorithm.searching : canFind;
            import std.traits : TemplateArgsOf;
            alias args = TemplateArgsOf!(SomeKey);
            debug static assert(args.length == 2,
                          "linear search for Nullable without nullValue is slower than default `this.contains()` and is not allowed");
            alias UnderlyingType = args[0];
            return length >= 1 && (cast(UnderlyingType[])_store).canFind!keyEqualPredFn(key.get());
        }
        else
        {
            foreach (const ref bin; _store)
                if (keyEqualPredFn(keyOf(bin), key))
                    return true;
            return false;
        }
    }

    /** Check if `element` is stored. Move found element to a hole if possible.
        Returns: `true` if element is present, `false` otherwise.
    */
    bool containsWithHoleMoving()(const scope K key) // template-lazy, `auto ref` here makes things slow
    {
        version(LDC) pragma(inline, true);

        assert(!key.isNull);
        static if (hasHoleableKey) { assert(!isHoleKeyConstant(key)); }
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }

        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(adjustKeyType(key));
        // TODO: update holes
        return (hitIndex != _store.length &&
                isOccupiedAtIndex(hitIndex));
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key
     * (set-case).
     *
     * If `element` is a nullable type and it is null an `AssertError` is thrown.
     */
    InsertionStatus insert()(const T element) @trusted // template-lazy. need `T` to be `const` in `class` case
    {
        version(LDC) pragma(inline, true);

        assert(!keyOf(element).isNull);
        static if (hasHoleableKey) { debug assert(!isHoleKeyConstant(keyOf(element))); }
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }

        reserveExtra(1);
        size_t hitIndex = 0;
        static if (__traits(isCopyable, T))
            return insertWithoutGrowth(element, hitIndex);
        else
            return insertWithoutGrowth(move(*cast(T*)&element), hitIndex);
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key
     * (set-case).
     *
     * If `element` is a nullable type and it is null an `AssertError` is thrown.
     *
     * Returns: reference to existing element if present, otherwise new `element`.
     *
     * Can be used for implementing, for instance, caching of typically strings.
     */
    ref T insertAndReturnElement(SomeElement)(scope SomeElement element) return // template-lazy
    {
        version(LDC) pragma(inline, true);

        assert(!keyOf(element).isNull);
        static if (hasHoleableKey) { debug assert(!isHoleKeyConstant(cast(K)adjustKeyType(keyOf(element)))); }
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }

        reserveExtra(1);
        static if (__traits(isCopyable, SomeElement))
            const hitIndex = insertWithoutGrowthNoStatus(element);
        else
            const hitIndex = insertWithoutGrowthNoStatus(move(element));
        return _store[hitIndex];
    }

    /** Insert `elements`, all being either a key-value (map-case) or a just a key (set-case).
     */
    void insertN(R)(R elements) @trusted
    if (isIterable!R &&
        __traits(isCopyable, T))           // TODO: support uncopyable T?
    {
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        import std.range.primitives : hasLength;
        static if (hasLength!R)
            reserveExtra(elements.length); // might create unused space in `_store` store
        foreach (element; elements)
        {
            static if (!hasLength!R)
                reserveExtra(1);
            static if (hasIndirections!T)
                insertWithoutGrowthNoStatus(element);
            else
                insertWithoutGrowthNoStatus(*cast(Unqual!T*)&element);
        }
    }

    /// Is `true` iff in-place rehashing during growth should be performed.
    enum bool growInPlaceFlag = false; // TODO: warning growInPlaceWithCapacity is buggy

    /// Numerator for grow scale.
    enum growScaleP = 3;
    /// Denominator for grow scale.
    enum growScaleQ = 2;

    /** Reserve rom for `extraCapacity` number of extra buckets. */
    void reserveExtra(size_t extraCapacity) // not template-lazy
    {
        version(LDC) pragma(inline, true);
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        immutable newCapacity = (_count + extraCapacity)*growScaleP/growScaleQ;
        if (newCapacity > _store.length)
            growWithNewCapacity(newCapacity);
    }

    /// Grow (rehash) to make for `newCapacity` number of elements.
    private void growWithNewCapacity()(size_t newCapacity) // template-lazy
    {
        version(LDC) pragma(inline, true);
        version(showEntries) dbg(__FUNCTION__, " newCapacity:", newCapacity);
        version(unittest) assert(newCapacity > _store.length);
        static if (__traits(hasMember, Allocator, "reallocate"))
        {
            static if (growInPlaceFlag)
                growInPlaceWithCapacity(newCapacity);
            else
                growStandardWithNewCapacity(newCapacity);
        }
        else
            growStandardWithNewCapacity(newCapacity);
    }

    private void tagAsLazilyDeletedElementAtIndex(size_t index)
    {
        version(LDC) pragma(inline, true);

        // key
        static if (useSmallLinearSearch)
            if (_store.length * T.sizeof <= linearSearchMaxSize)
            {
                keyOf(_store[index]).nullify();
                goto done;
            }

        static if (hasHoleableKey)
            keyOf(_store[index]) = holeKeyConstant;
        else
        {
            keyOf(_store[index]).nullify();
            tagHoleAtIndex(index);
        }

    done:

        // value
        static if (hasValue)
        {
            static if (hasElaborateDestructor!V) // if we should clear all
                .destroy(valueOf(_store[index]));
            static if (mustAddGCRange!V) // if we should clear all
                valueOf(_store[index]) = V.init; // avoid GC mark-phase dereference
        }
    }

    private void insertElementAtIndex(SomeElement)(scope SomeElement element, size_t index) @trusted // template-lazy
    {
        version(LDC) pragma(inline, true);
        static if (isSlice!SomeElement &&
                   !is(typeof(SomeElement.init[0]) == immutable))
        {
            /* key is an array of non-`immutable` elements which cannot safely
             * be stored because keys must be immutable for hashing to work
             * properly, therefore duplicate */
            keyOf(_store[index]) = element.idup;
        }
        else
        {
            static if (__traits(isCopyable, SomeElement))
                _store[index] = element;
            else
            {
                static if (__traits(isCopyable, K))
                    keyOf(_store[index]) = keyOf(element);
                else
                    move(keyOf(element),
                         keyOf(_store[index]));

                static if (hasValue)
                {
                    import core.lifetime : moveEmplace;
                    moveEmplace(valueOf(element),
                                valueOf(_store[index]));
                }
            }
        }
    }

    /** Rehash elements in-place. */
    private void rehashInPlace()() @trusted // template-lazy
    {
        version(showEntries) dbg(__FUNCTION__);

        import core.bitop : bts, bt;
        import nxt.array_help : makeZeroedBitArray, wordCountOfBitCount;

        size_t* dones = makeZeroedBitArray!Allocator(_store.length);

        foreach (immutable doneIndex; 0 .. _store.length)
        {
            if (bt(dones, doneIndex)) { continue; } // if _store[doneIndex] continue
            if (isOccupiedAtIndex(doneIndex))
            {
                import core.lifetime : moveEmplace;
                T currentElement = void;

                // TODO: functionize:
                moveEmplace(_store[doneIndex], currentElement);
                static if (isInstanceOf!(Nullable, K))
                    keyOf(_store[doneIndex]).nullify(); // `moveEmplace` doesn't init source of type Nullable

                while (true)
                {
                    alias pred = (const scope index,
                                  const scope auto ref element) => (!isOccupiedAtIndex(index) || // free slot
                                                                    !bt(dones, index)); // or a not yet replaced element
                    static if (usePrimeCapacity)
                        immutable hitIndex = xxx;
                    else
                        immutable hitIndex = _store[].triangularProbeFromIndex!(pred, assumeNonFullHaystack)(keyToIndex(keyOf(currentElement)));
                    assert(hitIndex != _store.length, "no free slot");

                    bts(dones, hitIndex); // _store[hitIndex] will be at it's correct position

                    if (isOccupiedAtIndex(doneIndex))
                    {
                        T nextElement = void;

                        // TODO: functionize:
                        moveEmplace(_store[hitIndex], nextElement); // save non-free slot
                        static if (isInstanceOf!(Nullable, K))
                            keyOf(_store[hitIndex]).nullify(); // `moveEmplace` doesn't init source of type Nullable

                        moveEmplace(currentElement, _store[hitIndex]);
                        moveEmplace(nextElement, currentElement);
                    }
                    else // if no free slot
                    {
                        moveEmplace(currentElement, _store[hitIndex]);
                        break; // inner iteration is finished
                    }
                }
            }
            bts(dones, doneIndex); // _store[doneIndex] is at it's correct position
        }

        Allocator.deallocate(cast(void[])(dones[0 .. wordCountOfBitCount(_store.length)]));

        static if (!hasHoleableKey)
            clearHoles();
    }

    /** Grow (with rehash) store in-place making room for `minimumCapacity` number of elements.
     */
    private void growInPlaceWithCapacity()(size_t minimumCapacity) @trusted // template-lazy
    {
        assert(minimumCapacity > _store.length);

        static if (usePrimeCapacity)
            const newCapacity = ceilingPrime(minimumCapacity, _primeIndex);
        else
            const newCapacity = nextPow2(minimumCapacity);

        immutable newByteCount = T.sizeof*newCapacity;

        const oldStorePtr = _store.ptr;
        immutable oldLength = _store.length;

        auto rawStore = cast(void[])_store;
        if (Allocator.reallocate(rawStore, newByteCount))
        {
            _store = cast(T[])rawStore;
            static if (mustAddGCRange!T)
            {
                if (oldStorePtr !is null)
                    gc_removeRange(oldStorePtr); // `gc_removeRange` fails for null input
                gc_addRange(_store.ptr, newByteCount);
            }

            static if (!hasHoleableKey)
                if (_holesPtr)
                    _holesPtr = makeReallocatedBitArrayZeroPadded!Allocator(_holesPtr,
                                                                            oldLength,
                                                                            _store.length);

            // TODO: make this an array operation `nullifyAll` or `nullifyN`
            foreach (ref bin; _store[oldLength .. newCapacity])
                keyOf(bin).nullify(); // move this `init` to reallocate() above?

            rehashInPlace();
        }
        else
            assert(0, "couldn't reallocate bin");
    }

    /** Grow (rehash) store to make room for `newCapacity` number of elements.
     */
    private void growStandardWithNewCapacity()(size_t newCapacity) // template-lazy
    {
        version(LDC) pragma(inline, true); // LDC needs this or to prevent 10x performance regression in contains()
        version(showEntries) dbg(__FUNCTION__, " newCapacity:", newCapacity);
        version(unittest) assert(newCapacity > _store.length);
        auto next = typeof(this).withCapacity(newCapacity);
        foreach (immutable index, ref bin; _store)
            if (isOccupiedAtIndex(index))
            {
                next.insertMoveWithoutGrowth(bin); // value is zeroed but
                static if (!hasHoleableKey)
                    keyOf(bin).nullify(); // keyC must zeroed
            }
        move(next, this);
    }

    private InsertionStatus insertWithoutGrowth(SomeElement)(const scope SomeElement element, // template-lazy
                                                             out size_t hitIndex) @trusted
    {
        version(LDC) pragma(inline, true);
        version(unittest)
        {
            assert(!keyOf(element).isNull);
            static if (hasHoleableKey) { assert(!isHoleKeyConstant(adjustKeyType(keyOf(element)))); }
        }

        size_t holeIndex = size_t.max; // first hole index to written to if hole found
        immutable hitIndexPrel = indexOfKeyOrVacancyAndFirstHole(keyOf(element), holeIndex);
        if (hitIndexPrel == _store.length || // keys miss and holes may have filled all empty slots
            keyOf(_store[hitIndexPrel]).isNull) // just key miss but a hole may have been found on the way
        {
            immutable hasHole = holeIndex != size_t.max; // hole was found along the way

            if (hasHole)
                hitIndex = holeIndex; // pick hole instead
            else
                hitIndex = hitIndexPrel; // normal hit

            version(unittest) assert(hitIndex != _store.length, "no null or hole slot");

            static if (__traits(isCopyable, SomeElement))
                insertElementAtIndex(*cast(SomeElement*)&element, hitIndex);
            else
                insertElementAtIndex(move(*cast(SomeElement*)&element), hitIndex);

            static if (!hasHoleableKey)
                if (hasHole)
                    untagHoleAtIndex(hitIndex);

            _count = _count + 1;
            return InsertionStatus.added;
        }
        else
            hitIndex = hitIndexPrel;

        static if (hasValue)
        {
            static if (__traits(isStaticArray, V))
                // identity comparison of static arrays implicitly coerces them
                // to slices, which are compared by reference, so don't use !is here
                immutable valueDiffers = (valueOf(element) !=
                                          valueOf(_store[hitIndexPrel])); // only value changed
            else
                immutable valueDiffers = (valueOf(element) !is
                                          valueOf(_store[hitIndexPrel])); // only value changed

            if (valueDiffers) // only value changed
            {
                move(valueOf(*cast(SomeElement*)&element),
                     valueOf(_store[hitIndexPrel])); // value is defined so overwrite it
                return InsertionStatus.modified;
            }
        }
        return InsertionStatus.unmodified;
    }

    private size_t insertWithoutGrowthNoStatus(SomeElement)(const scope SomeElement element) @trusted // template-lazy
    {
        version(LDC) pragma(inline, true);
        version(unittest)
        {
            assert(!keyOf(element).isNull);
            static if (hasHoleableKey) { assert(!isHoleKeyConstant(adjustKeyType(keyOf(element)))); }
        }

        size_t hitIndex = 0;
        size_t holeIndex = size_t.max; // first hole index to written to if hole found
        immutable hitIndexPrel = indexOfKeyOrVacancyAndFirstHole(adjustKeyType(keyOf(element)), holeIndex);
        if (hitIndexPrel == _store.length || // keys miss and holes may have filled all empty slots
            keyOf(_store[hitIndexPrel]).isNull) // just key miss but a hole may have been found on the way
        {
            immutable hasHole = holeIndex != size_t.max; // hole was found along the way
            if (hasHole)
                hitIndex = holeIndex; // pick hole instead
            else
                hitIndex = hitIndexPrel; // normal hit

            version(unittest) assert(hitIndex != _store.length, "no null or hole slot");

            static if (__traits(isCopyable, SomeElement))
                insertElementAtIndex(*cast(SomeElement*)&element, hitIndex);
            else
                insertElementAtIndex(move(*cast(SomeElement*)&element), hitIndex);

            static if (!hasHoleableKey)
                if (hasHole) { untagHoleAtIndex(hitIndex); }

            _count = _count + 1;
            return hitIndex;
        }
        else
            hitIndex = hitIndexPrel;

        static if (hasValue)
            // modify existing value
            move(valueOf(*cast(SomeElement*)&element),
                 valueOf(_store[hitIndexPrel])); // value is defined so overwrite it

        return hitIndex;
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    private InsertionStatus insertMoveWithoutGrowth()(ref T element) // template-lazy
    {
        version(LDC) pragma(inline, true);
        size_t hitIndex = 0;
        return insertWithoutGrowth(move(element), hitIndex);
    }

    static if (hasValue)
    {
        /** Insert or replace `value` at `key`. */
        InsertionStatus insert()(K key, V value) // template-lazy
        {
            pragma(inline, true); // LDC must have this
            static if (__traits(isCopyable, K))
            {
                static if (__traits(isCopyable, V))
                    return insert(T(key, value));
                else
                    return insert(T(key, move(value)));
            }
            else
            {
                static if (__traits(isCopyable, V))
                    return insert(T(move(key), value));
                else
                    return insert(T(move(key), move(value)));
            }
        }
    }

    static if (!hasValue)       // HashSet
    {
        scope const(K)* opBinaryRight(string op, SomeKey)(const scope SomeKey key) const return @trusted
        if (op == `in` &&
            isScopedKeyType!(typeof(key)))
        {
            version(D_Coverage) {} else pragma(inline, true);

            assert(!key.isNull);
            static if (hasHoleableKey) { assert(!isHoleKeyConstant(cast(K)adjustKeyType(key))); }

            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(adjustKeyType(key)); // cast scoped `key` is @trusted
            if (hitIndex != _store.length &&
                isOccupiedAtIndex(hitIndex))
                return &_store[hitIndex];
            else
                return null;
        }

        ref typeof(this) opOpAssign(string op, SomeKey)(const scope SomeKey key) return @trusted
        if (op == `~` &&        // binary assignment operator `~=`
            isScopedKeyType!(typeof(key)))
        {
            version(LDC) pragma(inline, true);
            reserveExtra(1);
            const hitIndex = insertWithoutGrowthNoStatus(key);
            return this;
        }

        /** Try to retrieve `class`-element of type `Class` constructed with
         * parameters `params`.
         *
         * Typically used to implement (polymorphic) caching of class-types
         * without the need for GG-allocating a temporary instance of a
         * `class`-element potentially already stored in `this` set.
         *
         * Polymorphic caching can be realized by setting `hasher` to
         * `hash_functions.hashOfPolymorphic`.
         */
        scope const(Class) tryGetElementFromCtorParams(Class, Params...)(scope Params params) const return @trusted
        if (is(Class : K))
        {
            import core.lifetime : emplace;
            void[__traits(classInstanceSize, Class)] tempNode_ = void;
            scope Class temp = emplace!(Class)(tempNode_, params);
            Class* hit = cast(Class*)(temp in this);

            static if (__traits(hasMember, Class, "__dtor"))
                temp.__dtor();

            if (hit)
            {
                auto typedHit = cast(typeof(return))*hit;
                assert(typedHit, "Expected class " ~ Class.stringof ~ " but got hit was of other type"); // TODO: give warning or throw
                return typedHit;
            }
            return null;
        }
    }

    static if (hasValue)        // HashMap
    {
        scope inout(V)* opBinaryRight(string op, SomeKey)(const scope SomeKey key) inout return @trusted // `auto ref` here makes things slow
        if (op == `in` &&
            isScopedKeyType!(SomeKey))
        {
            version(LDC) pragma(inline, true);

            // pragma(msg, SomeKey, " => ", K);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(const(K))adjustKeyType(key)); // cast scoped `key` is @trusted

            if (hitIndex != _store.length &&
                isOccupiedAtIndex(hitIndex))
                return cast(typeof(return))&_store[hitIndex].value;
            else
                return null;
        }

        /// Indexing.
        scope ref inout(V) opIndex(SomeKey)(const scope SomeKey key) inout return @trusted // `auto ref` here makes things slow. TODO: @nogc
        if (isScopedKeyType!(typeof(key)))
        {
            version(LDC) pragma(inline, true);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(adjustKeyType(key)); // cast scoped `key` is @trusted
            if (hitIndex != _store.length &&
                isOccupiedAtIndex(hitIndex))
                return _store[hitIndex].value;
            import core.exception : RangeError;
            throw new RangeError("Key not found"); // TODO: use assert instead?
        }

        /** Get value of `key` or `defaultValue` if `key` not present (and
         * therefore `nothrow`).
         *
         * Returns: value reference iff `defaultValue` is an l-value.
         *
         * TODO: make `defaultValue` `lazy` when that can be `nothrow`
         */
        auto ref inout(V) get()(const scope K key, // template-lazy
                                auto ref inout(V) defaultValue) inout
        {
            version(LDC) pragma(inline, true);
            if (auto valuePtr = key in this)
                return *valuePtr;
            else
                return defaultValue;
        }

        /** Get reference to `key`-part of stored element at `key`, if present,
         * otherwise return `defaultKey`.
         *
         * Used to implement caching inside the key part of a map.
         */
        ref const(K) getKeyRef(SomeKey)(const scope SomeKey key, // template-lazy
                                        ref const(K) defaultKey) const return @trusted @nogc
        if (isScopedKeyType!(SomeKey))
        {
            version(LDC) pragma(inline, true);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(adjustKeyType(key)); // cast scoped `key` is @trusted
            if (hitIndex != _store.length &&
                isOccupiedAtIndex(hitIndex))
                return _store[hitIndex].key;
            return defaultKey;
        }

        /** Supports the syntax `aa[key] = value;`.
         */
        ref V opIndexAssign()(V value, K key) // template-lazy. TODO: return scope
        {
            version(LDC) pragma(inline, true);
            assert(!key.isNull);

            static if (hasHoleableKey) { debug assert(!isHoleKeyConstant(key)); }
            static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }

            reserveExtra(1);

            static if (__traits(isCopyable, K))
            {
                static if (__traits(isCopyable, V))
                    const hitIndex = insertWithoutGrowthNoStatus(T(key, value));
                else
                    const hitIndex = insertWithoutGrowthNoStatus(T(key, move(value)));
            }
            else
            {
                static if (__traits(isCopyable, V))
                    const hitIndex = insertWithoutGrowthNoStatus(T(move(key), value));
                else
                    const hitIndex = insertWithoutGrowthNoStatus(T(move(key), move(value)));
            }

            return _store[hitIndex].value;
        }

        ref V opIndexOpAssign(string op, Rhs)(Rhs rhs, K key) // TODO: return scope
        // if (true)               // TODO: pre-check that mixin will work
        {
            // pragma(msg, "opIndexOpAssign: Key:", K, " Value:", V, " Rhs:", Rhs, " op:", op);
            assert(!key.isNull);
            static if (hasHoleableKey) { debug assert(!isHoleKeyConstant(key)); }
            static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }

            reserveExtra(1);

            size_t holeIndex = size_t.max; // first hole index to written to if hole found
            immutable hitIndex = indexOfKeyOrVacancyAndFirstHole(key, holeIndex);
            if (hitIndex == _store.length || // keys miss and holes may have filled all empty slots
                keyOf(_store[hitIndex]).isNull) // just key miss but a hole may have been found on the way
            {
                immutable hasHole = holeIndex != size_t.max; // hole was found along the way
                const index = (hasHole ?
                               holeIndex : // pick hole instead
                               hitIndex); // normal hit
                version(unittest) assert(index != _store.length, "no null or hole slot");
                static if (__traits(isCopyable, K))
                {
                    static if (op == "~" ||
                               op == "+" ||
                               op == "*")
                    {
                        static if (is(V : Rhs[])) // isDynamicArray of `Rhs`
                            insertElementAtIndex(T(key, [rhs]), // TODO: if `V(rhs)` is not supported use `V.init` followed by `OP= rhs`
                                                 index);
                        else
                            // dbg("opIndexOpAssign-new: k:", key, " rhs:", rhs);
                            insertElementAtIndex(T(key, V(rhs)), // TODO: if `V(rhs)` is not supported use `V.init` followed by `OP= rhs`
                                                 index);
                    }
                    else
                        static assert(0, "Handel op " ~ op);
                }
                else
                    static assert(0, "Handle uncopyable key " ~ K.stringof);
                    // insertElementAtIndex(move(*cast(SomeElement*)&element), index);

                static if (!hasHoleableKey)
                    if (hasHole) { untagHoleAtIndex(index); }

                _count = _count + 1;
                return _store[index].value;
            }
            else                // `key`-hit at index `hitIndex`
            {
                // dbg("opIndexOpAssign-mod: k:", key, " rhs:", rhs);
                mixin(`return _store[hitIndex].value ` ~ op ~ `= rhs;`); // modify existing value
            }
        }
    }

    /** Remove `element`.
     * Returns: `true` if element was removed, `false` otherwise.
     */
    bool remove(SomeKey)(const scope SomeKey key) scope // template-lazy
    if (isScopedKeyType!(typeof(key)))
    {
        version(LDC) pragma(inline, true);
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }

        static if (useSmallLinearSearch)
            if (_store.length * T.sizeof <= linearSearchMaxSize)
            {
                foreach (const index, const ref element; _store) // linear search is faster for small arrays
                    if (keyEqualPredFn(keyOf(element), key))
                    {
                        tagAsLazilyDeletedElementAtIndex(index);
                        _count = _count - 1;
                        return true;
                    }
                return false;
            }

        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(const(K))adjustKeyType(key));
        if (hitIndex != _store.length &&
            isOccupiedAtIndex(hitIndex))
        {
            tagAsLazilyDeletedElementAtIndex(hitIndex);
            _count = _count - 1;
            return true;
        }
        return false;
    }

    /** Remove all elements matching `keys` followed by a rehash.
     *
     * Returns: number of elements that were removed.
     */
    version(none)
    {
        import nxt.traits_ex : isRefIterable;
        import std.range.primitives : front;

        size_t rehashingRemoveN(Keys)(const scope Keys keys) // template-lazy
        if (isRefIterable!Keys &&
            is(typeof(Keys.front == K.init)))
        {
            static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
            rehash!("!a.isNull && keys.canFind(a)")(); // TODO: make this work
            return 0;
        }
    }

    /// Check if empty.
    @property bool empty() const { return _count == 0; }

    /// Get length (read-only).
    @property size_t length() const { return _count; }

    /// Get bin count.
    @property size_t binCount() const { return _store.length; }

    /** Returns: get total probe count for all elements stored. */
    size_t totalProbeCount()() const // template-lazy
    {
        version(LDC) pragma(inline, true); // LDC needs this or to prevent 10x performance regression in contains()

        static if (hasValue)
            auto range = byKeyValue(this);
        else
            auto range = byElement(this);

        typeof(return) totalCount = 0;

        foreach (const ref currentElement; range)
        {
            static if (__traits(isCopyable, T))
                /* don't use `auto ref` for copyable `T`'s to prevent
                 * massive performance drop for small elements when compiled
                 * with LDC. TODO: remove when LDC is fixed. */
                alias pred = (const scope element) => (keyEqualPredFn(keyOf(element),
                                                                      keyOf(currentElement)));
            else
                alias pred = (const scope auto ref element) => (keyEqualPredFn(keyOf(element),
                                                                               keyOf(currentElement)));

            static if (usePrimeCapacity)
                const probeCount = xxx;
            else
                const probeCount = triangularProbeCountFromIndex!(pred)(_store[], keyToIndex(keyOf(currentElement)));

            totalCount += probeCount;
        }
        return totalCount;
    }

    /** Returns: average probe count for all elements stored. */
    double averageProbeCount()() const // template-lazy
    {
        return cast(typeof(return))totalProbeCount/length;
    }

    /** Unsafe access to raw store.
     *
     * Needed by wrapper containers such as `SSOOpenHashSet`.
     */
    inout(T)[] rawStore() inout @system pure nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _store;
    }

    static if (hasHoleableKey)
    {
        static bool isOccupiedBin(const ref T bin)
        {
            version(D_Coverage) {} else pragma(inline, true);
            if (keyOf(bin).isNull)
                return false;
            return !isHoleKeyConstant(keyOf(bin));
        }
    }

private:
    static if (hasFunctionAttributes!(Allocator.allocate, "@nogc"))
    {
        import nxt.gc_traits : NoGc;
        @NoGc T[] _store;        // one element per bin
    }
    else
        T[] _store;              // one element per bin

    static if (usePrimeCapacity)
        PrimeIndex _primeIndex = PrimeIndex.init;

    static if (!hasHoleableKey)
    {
        static if (hasFunctionAttributes!(Allocator.allocate, "@nogc"))
            @NoGc size_t* _holesPtr; // bit array describing which bin elements that has been removed (holes)
        else
            size_t* _holesPtr; // bit array describing which bin elements that has been removed (holes)
    }

    static if (borrowChecked)
    {
        debug // use Rust-style borrow checking at run-time
        {
            size_t _count;
            size_t _borrowCount;

            /// Number of bits needed to store number of read borrows.
            enum borrowCountBits = 8*borrowChecked.sizeof;

            /// Maximum value possible for `_borrowCount`.
            enum borrowCountMax = 2^^borrowCountBits - 1;

            version(none)
            {
                /// Number of bits needed to store number of read borrows.
                enum borrowCountBits = 24;

                /// Maximum value possible for `_borrowCount`.
                enum borrowCountMax = 2^^borrowCountBits - 1;

                import std.bitmanip : bitfields;
                mixin(bitfields!(size_t, "_count", 8*size_t.sizeof - borrowCountBits,
                                 uint, "_borrowCount", borrowCountBits,
                          ));
            }

            pragma(inline, true):
            @safe pure nothrow @nogc:

            @property
            {
                /// Returns: `true` iff `this` is borrowed (either read or write).
                bool isBorrowed() const { return _borrowCount >= 1; }

                /// Returns: number of borrowers of `this` (both read and write).
                auto borrowCount() const { return _borrowCount; }
            }

            /// Increase borrow count.
            void incBorrowCount()
            {
                assert(_borrowCount + 1 != borrowCountMax);
                _borrowCount = _borrowCount + 1;
            }

            /// Decrease borrow count.
            void decBorrowCount()
            {
                assert(_borrowCount != 0);
                _borrowCount = _borrowCount - 1;
            }
        }
        else
        {
            size_t _count;        // total number of non-null elements stored in `_store`
        }
    }
    else
    {
        size_t _count;        // total number of non-null elements stored in `_store`
    }

    /** Returns: bin index of `key`. */
    private size_t keyToIndex(SomeKey)(const scope SomeKey key) const @trusted
    {
        version(LDC) pragma(inline, true);

        /** Returns: current index mask from bin count `_store.length`. */
        static size_t powerOf2Mask(in size_t length)
        {
            version(unittest) {}
            else
            {
                version(D_Coverage) {} else pragma(inline, true);
            }
            immutable typeof(return) mask = length - 1;
            version(unittest)
            {
                debug import std.math : isPowerOf2;
                debug assert(isPowerOf2(length));
            }
            return mask;
        }

        static if (is(typeof(hasher(key)) == hash_t)) // for instance when hasher being `hashOf`
            const size_t hash = hasher(key);
        else static if (is(hasher == struct) || // such as `FNV`
                        is(hasher == class))
        {
            import nxt.digestion : hashOf2;
            const size_t hash = hashOf2!(hasher)(key);
        }
        else
            static assert(false, "Unsupported hasher of type " ~ typeof(hasher).stringof);

        static if (usePrimeCapacity)
            return moduloPrimeIndex(hash, _primeIndex);
        else
            return hash & powerOf2Mask(_store.length);
    }

    /** Find index to `key` if it exists or to first empty slot found, skipping
     * (ignoring) lazily deleted slots.
     */
    private size_t indexOfKeyOrVacancySkippingHoles(const scope K key) const @trusted scope // `auto ref` here makes things slow
    // TODO: if (...)
    {
        version(LDC) pragma(inline, true);
        version(unittest)
        {
            assert(!key.isNull);
            static if (hasHoleableKey) { assert(!isHoleKeyConstant(key)); }
        }

        static if (useSmallLinearSearch)
            if (_store.length * T.sizeof <= linearSearchMaxSize)
            {
                foreach (const index, const ref element; _store) // linear search is faster for small arrays
                    if ((keyOf(element).isNull ||
                         keyEqualPredFn(keyOf(element), key)))
                        return index;
                return _store.length;
            }

        static if (hasHoleableKey)
            alias pred = (const scope auto ref element) => (keyOf(element).isNull ||
                                                            keyEqualPredFn(keyOf(element), key));
        else
            alias pred = (const scope index,
                          const scope auto ref element) => (!hasHoleAtPtrIndex(_holesPtr, index) &&
                                                            (keyOf(element).isNull ||
                                                             keyEqualPredFn(keyOf(element), key)));

        static if (usePrimeCapacity)
            return xxx;
        else
            return _store[].triangularProbeFromIndex!(pred, assumeNonFullHaystack)(keyToIndex(key));
    }

    private size_t indexOfKeyOrVacancyAndFirstHole(const scope K key, // `auto ref` here makes things slow
                                                   ref size_t holeIndex) const @trusted scope
    {
        version(LDC) pragma(inline, true);
        version(unittest)
        {
            assert(!key.isNull);
            static if (hasHoleableKey) { assert(!isHoleKeyConstant(key)); }
        }

        static if (useSmallLinearSearch)
            if (_store.length * T.sizeof <= linearSearchMaxSize)
            {
                foreach (const index, const ref element; _store) // linear search is faster for small arrays
                    if ((keyOf(element).isNull ||
                         keyEqualPredFn(keyOf(element), key)))
                        return index;
                return _store.length;
            }

        static if (hasHoleableKey)
        {
            alias hitPred = (const scope auto ref element) => (keyOf(element).isNull ||
                                                               keyEqualPredFn(keyOf(element), key));
            alias holePred = (const scope auto ref element) => (isHoleKeyConstant(keyOf(element)));
        }
        else
        {
            alias hitPred = (const scope index,
                             const scope auto ref element) => (!hasHoleAtPtrIndex(_holesPtr, index) &&
                                                               (keyOf(element).isNull ||
                                                                keyEqualPredFn(keyOf(element), key)));
            alias holePred = (const scope index, // TODO: use only index
                              const scope auto ref element) => (hasHoleAtPtrIndex(_holesPtr, index));
        }

        static if (usePrimeCapacity)
            return xxx;
        else
            return _store[].triangularProbeFromIndexIncludingHoles!(hitPred, holePred, assumeNonFullHaystack)(keyToIndex(key), holeIndex);
    }

    /** Returns: `true` iff `index` indexes a non-null element, `false`
     * otherwise.
     */
    private bool isOccupiedAtIndex(size_t index) const
    {
        version(LDC) pragma(inline, true);
        version(unittest) assert(index < _store.length);
        if (keyOf(_store[index]).isNull) { return false; }
        static if (hasHoleableKey)
            return !isHoleKeyConstant(keyOf(_store[index]));
        else
            return !hasHoleAtPtrIndex(_holesPtr, index);
    }
}

/** Duplicate `src` into uninitialized `dst` ignoring prior destruction of `dst`.
 * TODO: move to more generic place
 */
static private void duplicateEmplace(T)(const scope ref T src,
                                        scope ref T dst) @system
{
    version(D_Coverage) {} else pragma(inline, true);
    import core.internal.traits : hasElaborateCopyConstructor;
    import std.traits : isBasicType, isInstanceOf;
    static if (!hasElaborateCopyConstructor!T)
    {
        import std.typecons : Nullable;
        static if (is(T == class) ||
                   is(T == string))
            dst = cast(T)src;
        else static if (isBasicType!T ||
                        isInstanceOf!(Nullable, T)) // `Nullable` types cannot be emplaced
            dst = src;
        else                    // TODO: can this case occur?
        {
            import core.internal.traits : Unqual;
            import core.lifetime : emplace;
            emplace(&dst, cast(Unqual!T)src);
        }
    }
    else static if (__traits(hasMember, T, "dup"))
    {
        import core.lifetime : emplace;
        // TODO: when `emplace` can handle src being an r-value of uncopyable types replace with: `emplace(&dst, src.dup);`
        emplace(&dst);
        dst = src.dup;
    }
    else
        debug static assert(0, "cannot duplicate a " ~ T.stringof);
}

/** L-value element reference (and in turn range iterator).
 */
static private struct LvalueElementRef(SomeMap)
{
    import std.traits : isMutable;
    debug static assert(isMutable!SomeMap, "SomeMap type should always be mutable");

    private SomeMap* _table;      // scoped access
    private size_t _binIndex;   // index to bin inside `table`
    private size_t _hitCounter; // counter over number of elements popped (needed for length)

    this(SomeMap* table) @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        this._table = table;
        static if (SomeMap.isBorrowChecked)
            debug
            {
                _table.incBorrowCount();
            }
    }

    ~this() @nogc @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        static if (SomeMap.isBorrowChecked)
            debug
            {
                _table.decBorrowCount();
            }
    }

    this(this) @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        static if (SomeMap.isBorrowChecked)
            debug
            {
                assert(_table._borrowCount != 0);
                _table.incBorrowCount();
            }
    }

    /// Check if empty.
    @property bool empty() const @safe pure nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _binIndex == _table.binCount;
    }

    /// Get number of element left to pop.
    @property size_t length() const @safe pure nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _table.length - _hitCounter;
    }

    @property typeof(this) save() // ForwardRange
    {
        version(D_Coverage) {} else pragma(inline, true);
        return this;
    }

    void popFront()
    {
        version(LDC) pragma(inline, true);
        assert(!empty);
        _binIndex += 1;
        findNextNonEmptyBin();
        _hitCounter += 1;
    }

    private void findNextNonEmptyBin()
    {
        version(D_Coverage) {} else pragma(inline, true);
        while (_binIndex != (*_table).binCount &&
               !(*_table).isOccupiedAtIndex(_binIndex))
            _binIndex += 1;
    }
}

/** R-value element reference (and in turn range iterator).
 *
 * Does need to do borrow-checking.
 */
static private struct RvalueElementRef(SomeMap)
{
    import std.traits : isMutable;
    debug static assert(isMutable!SomeMap, "SomeMap type should always be mutable");

    SomeMap _table;                // owned table
    size_t _binIndex;            // index to bin inside table
    size_t _hitCounter;    // counter over number of elements popped

    /// Check if empty.
    @property bool empty() const @safe pure nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _binIndex == _table.binCount;
    }

    /// Get number of element left to pop.
    @property size_t length() const @safe pure nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _table.length - _hitCounter;
    }

    void popFront()
    {
        version(LDC) pragma(inline, true);
        assert(!empty);
        _binIndex += 1;
        findNextNonEmptyBin();
        _hitCounter += 1;
    }

    private void findNextNonEmptyBin()
    {
        version(D_Coverage) {} else pragma(inline, true);
        while (_binIndex != _table.binCount &&
               !_table.isOccupiedAtIndex(_binIndex))
            _binIndex += 1;
    }
}

/** Hash set with in-place open-addressing, storing keys (elements) of type `K`.
 *
 * Reuse `OpenHashMap` with its V-type set to `void`.
 *
 * See_Also: `OpenHashMap`.
 */
alias OpenHashSet(K,
                  alias hasher = hashOf,
                  string keyEqualPred = defaultKeyEqualPredOf!K,
                  alias Allocator = Mallocator.instance,
                  bool borrowChecked = false,
                  bool useSmallLinearSearch = true,
                  bool usePrimeCapacity = false) =
OpenHashMap!(K, void, hasher, keyEqualPred, Allocator, borrowChecked, useSmallLinearSearch, usePrimeCapacity);

import std.traits : isInstanceOf;
import std.functional : unaryFun;

/** Remove all elements in `x` matching `pred`.
 *
 * TODO: make this generic for all iterable containers and move to
 * container_algorithm.
 */
size_t removeAllMatching(alias pred, SomeMap)(auto ref SomeMap x) @trusted
if (isInstanceOf!(OpenHashMap, SomeMap) && // TODO: generalize to `isSetOrMap`
    is(typeof((unaryFun!pred))))
{
    import nxt.nullable_traits : nullify;
    size_t removalCount = 0;
    foreach (immutable index, ref bin; x._store)
    {
        // TODO:
        // move to SomeMap.removeRef(bin) // uses: `offset = &bin - _store.ptr`
        // move to SomeMap.inplaceRemove(bin) // uses: `offset = &bin - _store.ptr`
        // or   to SomeMap.removeAtIndex(index)
        if (x.isOccupiedAtIndex(index) &&
            unaryFun!pred(bin))
        {
            x.tagAsLazilyDeletedElementAtIndex(index);
            removalCount += 1;
        }
    }
    x._count = x._count - removalCount;
    return removalCount;        // TODO: remove this return value
}

/** Returns: `x` eagerly filtered on `pred`.
    TODO: move to container_algorithm.d with more generic template restrictions
*/
SomeMap filtered(alias pred, SomeMap)(SomeMap x)
if (isInstanceOf!(OpenHashMap, SomeMap)) // TODO: generalize to `isSetOrMap`
{
    import core.lifetime : move;
    import std.functional : not;
    x.removeAllMatching!(not!pred); // `x` is a singleton (r-value) so safe to mutate
    return move(x);             // functional
}

/** Returns: `x` eagerly intersected with `y`.
    TODO: move to container_algorithm.d.
 */
auto intersectedWith(C1, C2)(C1 x, auto ref C2 y)
if (isInstanceOf!(OpenHashMap, C1) && // TODO: generalize to `isSetOrMap`
    isInstanceOf!(OpenHashMap, C2))   // TODO: generalize to `isSetOrMap`
{
    import core.lifetime : move;
    static if (__traits(isRef, y)) // y is l-value
        // @("complexity", "O(x.length)")
        return move(x).filtered!(_ => y.contains(_)); // only x can be reused
    else
    {
        /* both are r-values so reuse the shortest */
        // @("complexity", "O(min(x.length), min(y.length))")
        if (x.length < y.length)
            return move(x).filtered!(_ => y.contains(_)); // functional
        else
            return move(y).filtered!(_ => x.contains(_)); // functional
    }
}

/// exercise opEquals
@safe pure nothrow @nogc
unittest
{
    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;

    alias K = Nullable!(ulong, ulong.max);
    alias X = OpenHashSet!(K, FNV!(64, true));

    const n = 100;

    X a;
    foreach (const i_; 0 .. n)
    {
        const i = 1113*i_;           // insert in order
        assert(!a.contains(K(i)));
        assert(!a.containsUsingLinearSearch(K(i)));
        assert(a.insertAndReturnElement(K(i)) == K(i));
        assert(a.contains(K(i)));
        assert(a.containsUsingLinearSearch(K(i)));
    }

    X b;
    foreach (const i_; 0 .. n)
    {
        const i = 1113*(n - 1 - i_);   // insert in reverse
        assert(!b.contains(K(i)));
        assert(!b.containsUsingLinearSearch(K(i)));
        assert(b.insertAndReturnElement(K(i)) == K(i));
        assert(b.contains(K(i)));
        assert(b.containsUsingLinearSearch(K(i)));
    }

    assert(a == b);

    // bin storage must be deterministic
    () @trusted { assert(a._store != b._store); }();
}

@safe pure nothrow @nogc unittest
{
    import nxt.digestx.fnv : FNV;

    enum Pot { noun, verb }
    struct ExprPot
    {
        string expr;
        Pot pot;

        alias nullifier = expr;
        static immutable nullValue = typeof(this).init;
    }

    alias X = OpenHashSet!(ExprPot, FNV!(64, true));

    X x;

    const aa = "aa";

    // two keys with same contents but different place in memory
    const key1 = ExprPot(aa[0 .. 1], Pot.noun);
    const key2 = ExprPot(aa[1 .. 2], Pot.noun);

    assert(key1 == key2);
    assert(key1 !is key2);

    assert(!x.contains(key1));
    assert(!x.contains(key2));
    x.insert(key1);
    assert(x.contains(key1));
    assert(x.containsUsingLinearSearch(key1));
    assert(x.contains(key2));
    /* assert(x.containsUsingLinearSearch(key2)); */
    assert(key1 in x);
    assert(key2 in x);
}

/// `string` as key
@safe pure nothrow @nogc unittest
{
    version(showEntries) dbg();
    import nxt.container_traits : mustAddGCRange;
    import nxt.digestx.fnv : FNV;

    alias X = OpenHashSet!(string, FNV!(64, true));
    debug static assert(!mustAddGCRange!X);
    debug static assert(X.sizeof == 24); // dynamic arrays also `hasAddressLikeKey`

    auto x = X();

    auto testEscapeShouldFail()() @safe pure
    {
        X x;
        x.insert("a");
        return x.byElement;
    }

    auto testEscapeShouldFailFront()() @safe pure
    {
        X x;
        x.insert("a");
        return x.byElement.front;
    }

    assert(&"a"[0] is &"a"[0]); // string literals are store in common place

    const aa = "aa";

    // string slices are equal when elements are equal regardless of position
    // (.ptr) in memory
    assert(x.insertAndReturnElement(aa[0 .. 1]) !is "a");
    x.insert(aa[0 .. 1]);
    assert(x.insertAndReturnElement(aa[0 .. 1]) is aa[0 .. 1]);
    assert(x.contains(aa[1 .. 2]));
    assert(x.containsUsingLinearSearch(aa[1 .. 2]));

    const(char)[] aa_ = "aa";
    assert(x.contains(aa_[1 .. 2]));
    assert(x.containsUsingLinearSearch(aa_[1 .. 2]));
    assert(aa_[1 .. 2] in x);

    char[2] aa__; aa__ = "aa";
    assert(x.contains(aa__[1 .. 2]));
    assert(x.containsUsingLinearSearch(aa__[1 .. 2]));
    assert(aa__[1 .. 2] in x);

    const bb = "bb";

    assert(x.insertAndReturnElement(bb[0 .. 1]) is bb[0 .. 1]); // returns newly added ref
    assert(x.insertAndReturnElement(bb[0 .. 1]) !is "b");       // return other ref not equal new literal
    x.insert(bb[0 .. 1]);
    assert(x.contains(bb[1 .. 2]));
    assert(x.containsUsingLinearSearch(bb[1 .. 2]));

    x.remove(aa[0 .. 1]);
    assert(!x.contains(aa[1 .. 2]));
    assert(!x.containsUsingLinearSearch(aa[1 .. 2]));
    assert(x.contains(bb[1 .. 2]));
    assert(x.containsUsingLinearSearch(bb[1 .. 2]));

    x.remove(bb[0 .. 1]);
    assert(!x.contains(bb[1 .. 2]));
    assert(!x.containsUsingLinearSearch(bb[1 .. 2]));

    x.insert("a");
    x.insert("b");
    assert(x.contains("a"));
    assert(x.containsUsingLinearSearch("a"));
    assert(x.contains("b"));
    assert(x.containsUsingLinearSearch("b"));

    debug static assert(!__traits(compiles, { testEscapeShouldFail(); } ));
    // TODO: this should fail:
    // TODO: debug static assert(!__traits(compiles, { testEscapeShouldFailFront(); } ));
}

/// `string` as key
@safe pure nothrow unittest
{
    version(showEntries) dbg();
    import nxt.digestx.fnv : FNV;
    alias X = OpenHashSet!(string, FNV!(64, true));
    auto x = X();

    char[2] cc = "cc";          // mutable chars
    assert(x.insertAndReturnElement(cc[]) !is cc[]); // will allocate new slice

    const cc_ = "cc";           // immutable chars
    assert(x.insertAndReturnElement(cc_[]) !is cc[]); // will not allocate
}

/// array container as value type
@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    import std.typecons : Nullable;
    import nxt.container_traits : mustAddGCRange;
    import nxt.digestx.fnv : FNV;
    import nxt.array_help : s;
    version(showEntries) dbg();

    import nxt.dynamic_array : Array = DynamicArray;

    alias K = Nullable!(uint, uint.max);

    alias VE = Nullable!(uint, uint.max);
    alias V = OpenHashSet!(VE, FNV!(64, true));

    debug static assert(!mustAddGCRange!V);

    foreach (X; AliasSeq!(OpenHashMap!(K, V, FNV!(64, true))))
    {
        const VE n = 600;

        auto x = X();

        {                       // scoped range
            auto xkeys = x.byKey;
            assert(xkeys.length == 0);
            foreach (ref key; xkeys)
            {
                debug static assert(is(typeof(key) == const(K)));
                assert(0);
            }
            foreach (ref key; X().byKey)
            {
                debug static assert(is(typeof(key) == const(K)));
                assert(0);
            }
        }

        foreach (immutable i; 0 .. n)
        {
            assert(x.length == i);

            auto key = K(i);
            auto value = V.withElements([VE(i)].s);

            x[key] = value.dup;
            assert(x.length == i + 1);
            assert(x.contains(key));
            // TODO: assert(x.containsUsingLinearSearch(key));
            {
                auto valuePtr = key in x;
                assert(valuePtr && *valuePtr == value);
            }

            x.remove(key);
            assert(x.length == i);
            assert(!x.contains(key));
            assert(key !in x);

            x[key] = value.dup;
            assert(x.length == i + 1);
            assert(x.contains(key));
            {
                auto valuePtr = key in x;
                assert(valuePtr && *valuePtr == value);
            }
        }

        assert(x is x);

        x = x.dup;

        auto y = x.dup;
        assert(x !is y);
        assert(x.length == y.length);

        assert(y == x);
        assert(x == y);

        foreach (ref key; x.byKey)
        {
            assert(x.contains(key));
        }

        foreach (ref keyValue; x.byKeyValue)
        {
            assert(x.contains(keyValue.key));
            auto keyValuePtr = keyValue.key in x;
            assert(keyValuePtr &&
                   *keyValuePtr == keyValue.value);
        }

        foreach (immutable i; 0 .. n)
        {
            assert(x.length == n - i);

            auto key = K(i);
            auto value = V.withElements([VE(i)].s);

            assert(x.contains(key));
            {
                auto valuePtr = key in x;
                assert(valuePtr && *valuePtr == value);
            }

            x.remove(key);
            assert(!x.contains(key));
            assert(key !in x);
        }

        auto z = y.dup;
        assert(y == z);

        /* remove all elements in `y` using `removeAllMatching` and all elements
         * in `z` using `removeAllMatching` */
        foreach (immutable i; 0 .. n)
        {
            assert(y.length == n - i);
            assert(z.length == n - i);

            auto key = K(i);
            auto value = V.withElements([VE(i)].s);

            assert(y.contains(key));
            {
                auto valuePtr = key in y;
                assert(valuePtr && *valuePtr == value);
            }
            assert(z.contains(key));
            {
                auto valuePtr = key in z;
                assert(valuePtr && *valuePtr == value);
            }

            y.remove(key);
            assert(z.removeAllMatching!((const scope ref element) => element.key is key) == 1);
            assert(y == z);

            assert(!y.contains(key));
            assert(!z.contains(key));

            assert(key !in y);
            assert(key !in z);
        }
    }
}

/// r-value and l-value intersection
@safe pure nothrow @nogc unittest
{
    import core.lifetime : move;
    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;
    import nxt.array_help : s;

    version(showEntries) dbg();
    alias K = Nullable!(uint, uint.max);
    alias X = OpenHashSet!(K, FNV!(64, true));

    auto x = X();

    {                           // scoped range
        foreach (ref xe; x.byElement) { assert(0); }
    }

    auto x0 = X.init;
    assert(x0.length == 0);
    assert(x0._store.length == 0);
    assert(!x0.contains(K(1)));

    auto x1 = X.withElements([K(12)].s);
    assert(x1.length == 1);
    assert(x1.contains(K(12)));

    auto x2 = X.withElements([K(10), K(12)].s);
    assert(x2.length == 2);
    assert(x2.contains(K(10)));
    assert(x2.contains(K(12)));

    auto x3 = X.withElements([K(12), K(13), K(14)].s);
    assert(x3.length == 3);
    assert(x3.contains(K(12)));
    assert(x3.contains(K(13)));
    assert(x3.contains(K(14)));

    auto z = X.withElements([K(10), K(12), K(13), K(15)].s);
    assert(z.length == 4);
    assert(z.contains(K(10)));
    assert(z.contains(K(12)));
    assert(z.contains(K(13)));
    assert(z.contains(K(15)));

    auto y = move(z).intersectedWith(x2);
    assert(y.length == 2);
    assert(y.contains(K(10)));
    assert(y.contains(K(12)));
    assert(y.containsUsingLinearSearch(K(10)));
    assert(y.containsUsingLinearSearch(K(12)));
}

/// r-value and r-value intersection
@safe pure nothrow @nogc unittest
{
    version(showEntries) dbg();

    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;
    import nxt.array_help : s;

    alias K = Nullable!(uint, uint.max);
    alias X = OpenHashSet!(K, FNV!(64, true));

    auto y = X.withElements([K(10), K(12), K(13), K(15)].s).intersectedWith(X.withElements([K(12), K(13)].s));
    assert(y.length == 2);
    assert(y.contains(K(12)));
    assert(y.contains(K(13)));
    assert(y.containsUsingLinearSearch(K(12)));
    assert(y.containsUsingLinearSearch(K(13)));
}

/** Returns: `x` eagerly intersected with `y`.
    TODO: move to container_algorithm.d.
 */
auto intersectWith(C1, C2)(ref C1 x,
                           auto ref const(C2) y)
if (isInstanceOf!(OpenHashMap, C1) &&
    isInstanceOf!(OpenHashMap, C2))
{
    return x.removeAllMatching!(_ => !y.contains(_));
}

/// r-value and l-value intersection
@safe pure nothrow @nogc unittest
{
    version(showEntries) dbg();

    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;
    import nxt.array_help : s;

    alias K = Nullable!(uint, uint.max);
    alias X = OpenHashSet!(K, FNV!(64, true));

    auto x = X.withElements([K(12), K(13)].s);
    auto y = X.withElements([K(10), K(12), K(13), K(15)].s);
    y.intersectWith(x);
    assert(y.length == 2);
    assert(y.contains(K(12)));
    assert(y.containsUsingLinearSearch(K(12)));
    assert(y.contains(K(13)));
    assert(y.containsUsingLinearSearch(K(13)));
}

/// Range over elements of l-value instance of this.
static struct ByLvalueElement(SomeMap) // public for now because this is needed in `knet.zing.Zing.EdgesOfRels`
{
pragma(inline, true):
    // TODO: functionize
    import std.traits : isMutable;
    static if (isAddress!(SomeMap.ElementType)) // for reference types
    {
        /// Get reference to front element.
        @property scope SomeMap.ElementType front()() return @trusted
        {
            // cast to head-const for class key
            return (cast(typeof(return))_table._store[_binIndex]);
        }
    }
    else
    {
        /// Get reference to front element.
        @property scope auto ref front() return @trusted
        {
            return *(cast(const(SomeMap.ElementType)*)&_table._store[_binIndex]); // propagate constnes
        }
    }
    import core.internal.traits : Unqual;
    // unqual to reduce number of instantations of `LvalueElementRef`
    public LvalueElementRef!(Unqual!SomeMap) _elementRef;
    alias _elementRef this;
}

/// Range over elements of r-value instance of this.
static private struct ByRvalueElement(SomeMap)
{
    @disable this(this);
pragma(inline, true):
    static if (isAddress!(SomeMap.ElementType)) // for reference types
    {
        /// Get reference to front element.
        @property scope SomeMap.ElementType front()() return @trusted
        {
            // cast to head-const for class key
            return cast(typeof(return))_table._store[_binIndex];
        }
    }
    else
    {
        /// Get reference to front element.
        @property scope auto ref front() return
        {
            return *(cast(const(SomeMap.ElementType)*)&_table._store[_binIndex]); // propagate constnes
        }
    }
    import core.internal.traits : Unqual;
    public RvalueElementRef!(Unqual!SomeMap) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the elements of `c` in undefined order.
 */
auto byElement(SomeMap)(auto ref return SomeMap c) @trusted
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    !SomeMap.hasValue)
{
    import core.internal.traits : Unqual;
    alias M = Unqual!SomeMap;
    alias C = const(SomeMap);        // be const for now
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByLvalueElement!C((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import core.lifetime : move;
        auto result = ByRvalueElement!C((RvalueElementRef!(M)(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}
alias range = byElement;        // EMSI-container naming

static private struct ByKey_lvalue(SomeMap)
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    @property const scope auto ref front() return // key access must be const, TODO: auto ref => ref K
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _table._store[_binIndex].key;
    }
    import core.internal.traits : Unqual;
    public LvalueElementRef!(Unqual!SomeMap) _elementRef;
    alias _elementRef this;
}

static private struct ByKey_rvalue(SomeMap)
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    @property const scope auto ref front() return // key access must be const, TODO: auto ref => ref K
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _table._store[_binIndex].key;
    }
    import core.internal.traits : Unqual;
    public RvalueElementRef!(Unqual!SomeMap) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the keys of `c` in undefined order.
 */
auto byKey(SomeMap)(auto ref /*TODO: return*/ SomeMap c) @trusted
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    import core.internal.traits : Unqual;
    alias M = Unqual!SomeMap;
    alias C = const(SomeMap);        // be const
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByKey_lvalue!C((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import core.lifetime : move;
        auto result = ByKey_rvalue!C((RvalueElementRef!M(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

static private struct ByValue_lvalue(SomeMap)
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    @property scope auto ref front() return @trusted // TODO: auto ref => ref V
    {
        version(D_Coverage) {} else pragma(inline, true);
        // TODO: functionize
        import std.traits : isMutable;
        static if (isMutable!(SomeMap)) // TODO: can this be solved without this `static if`?
        {
            alias E = SomeMap.ValueType;
        }
        else
        {
            alias E = const(SomeMap.ValueType);
        }
        return *(cast(E*)&_table._store[_binIndex].value);
    }
    import core.internal.traits : Unqual;
    public LvalueElementRef!(Unqual!SomeMap) _elementRef;
    alias _elementRef this;
}

static private struct ByValue_rvalue(SomeMap)
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    @property scope auto ref front() return @trusted // TODO: auto ref => ref V
    {
        version(D_Coverage) {} else pragma(inline, true);
        // TODO: functionize
        import std.traits : isMutable;
        static if (isMutable!(SomeMap)) // TODO: can this be solved without this `static if`?
        {
            alias E = SomeMap.ValueType;
        }
        else
        {
            alias E = const(SomeMap.ValueType);
        }
        return *(cast(E*)&_table._store[_binIndex].value);
    }
    import core.internal.traits : Unqual;
    public RvalueElementRef!(Unqual!SomeMap) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the values of `c` in undefined order.
 */
auto byValue(SomeMap)(auto ref return SomeMap c) @trusted
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    import core.internal.traits : Unqual;
    import std.traits : isMutable;
    alias M = Unqual!SomeMap;
    alias C = const(SomeMap);
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByValue_lvalue!SomeMap((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import core.lifetime : move;
        auto result = ByValue_rvalue!C((RvalueElementRef!M(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

static private struct ByKeyValue_lvalue(SomeMap)
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    @property scope auto ref front() return @trusted // TODO: auto ref => ref T
    {
        version(D_Coverage) {} else pragma(inline, true);
        // TODO: functionize
        import std.traits : isMutable;
        static if (isMutable!(SomeMap))
        {
            alias E = SomeMap.KeyValueType;
        }
        else
        {
            alias E = const(SomeMap.T);
        }
        return *(cast(E*)&_table._store[_binIndex]);
    }
    import core.internal.traits : Unqual;
    public LvalueElementRef!(Unqual!SomeMap) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the key-value-pairs of `c` in undefined order.
 */
auto byKeyValue(SomeMap)(auto ref return SomeMap c) @trusted
if (isInstanceOf!(OpenHashMap, SomeMap) &&
    SomeMap.hasValue)
{
    import core.internal.traits : Unqual;
    alias M = Unqual!SomeMap;
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByKeyValue_lvalue!SomeMap((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import core.lifetime : move;
        auto result = ByKeyValue_rvalue!SomeMap((RvalueElementRef!M(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

/// make range from l-value and r-value. element access is always const
@safe pure unittest
{
    version(showEntries) dbg();

    import core.exception : RangeError, AssertError;
    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;
    import nxt.array_help : s;
    debug import std.exception : assertThrown, assertNotThrown;

    import std.algorithm.searching : count;
    alias K = Nullable!(uint, uint.max);
    alias X = OpenHashSet!(K,
                           FNV!(64, true),
                           defaultKeyEqualPredOf!K,
                           Mallocator.instance,
                           true);

    auto k11 = K(11);
    auto k22 = K(22);
    auto k33 = K(33);
    auto ks = [k11, k22, k33].s;
    auto k44 = K(44);

    // mutable
    auto x = X.withElements(ks);
    assert(!x.contains(k44));
    assert(!x.containsUsingLinearSearch(k44));
    assert(x.length == 3);

    assert(x.byElement.count == x.length);
    foreach (e; x.byElement)    // from l-value
    {
        debug static assert(is(typeof(e) == const(K))); // always const access

        // range invalidation forbidden:
        debug
        {
            assertThrown!AssertError(x.reserveExtra(1));  // range invalidation
            assertThrown!AssertError(x.clear());          // range invalidation
            assertThrown!AssertError(x.insert(k11));      // range invalidation
            assertThrown!AssertError(x.insertN([k11].s)); // range invalidation
            assertThrown!AssertError(x.remove(k11));      // range invalidation
        }

        // allowed
        assert(x.contains(e));
        assert(x.containsUsingLinearSearch(e));

        const eHit = e in x;
        assert(eHit);           // found
        assert(*eHit is e);     // and the value equals what we searched for

        const eDup = x.dup;     // duplication is `const` and allowed
    }

    // const
    const y = X.withElements(ks);
    assert(!x.contains(k44));
    assert(!x.containsUsingLinearSearch(k44));
    foreach (e; y.byElement)    // from l-value
    {
        auto z = y.byElement;   // ok to read-borrow again
        assert(y.contains(e));
        assert(y.containsUsingLinearSearch(e));
        debug static assert(is(typeof(e) == const(K)));
    }

    foreach (e; X.withElements([K(11)].s).byElement) // from r-value
    {
        assert(e == K(11));
        debug static assert(is(typeof(e) == const(K))); // always const access
    }
}

/// range checking
@safe pure unittest
{
    version(showEntries) dbg();
    import core.exception : RangeError, AssertError;
    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;
    debug import std.exception : assertThrown, assertNotThrown;
    immutable n = 11;

    alias K = Nullable!(uint, uint.max);
    alias V = uint;

    alias X = OpenHashMap!(K, V, FNV!(64, true));

    auto s = X.withCapacity(n);

    void dummy(ref V value) {}

    debug assertThrown!RangeError(dummy(s[K(0)]));

    foreach (immutable i; 0 .. n)
    {
        const k = K(i);
        s[k] = V(i);
        debug assertNotThrown!RangeError(dummy(s[k]));
    }

    foreach (immutable i; 0 .. n)
    {
        const k = K(i);
        assert(s.remove(k));
        debug assertThrown!RangeError(dummy(s[k]));
    }

    s[K(0)] = V.init;
    auto vp = K(0) in s;
    debug static assert(is(typeof(vp) == V*));
    assert((*vp) == V.init);

    assert(s.remove(K(0)));
    assert(K(0) !in s);

    X t;
    t.reserveExtra(4096);

    t.clear();
}

/// class as value
@safe pure unittest
{
    version(showEntries) dbg();
    import core.exception : RangeError, AssertError;
    import std.typecons : Nullable;
    debug import std.exception : assertThrown, assertNotThrown;
    import nxt.digestx.fnv : FNV;

    immutable n = 11;

    alias K = Nullable!(uint, uint.max);
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = OpenHashMap!(K, V, FNV!(64, true));

    auto s = X.withCapacity(n);

    void dummy(ref V value) {}

    debug assertThrown!RangeError(dummy(s[K(0)]));

    foreach (immutable i; 0 .. n)
    {
        const k = K(i);
        s[k] = new V(i);
        debug assertNotThrown!RangeError(dummy(s[k]));
    }

    // test range
    {
        auto sr = s.byKeyValue; // scoped range
        assert(sr.length == n);
        foreach (immutable i; 0 .. n)
        {
            sr.popFront();
            assert(sr.length == n - i - 1);
        }
    }

    foreach (immutable i; 0 .. n)
    {
        const k = K(i);
        assert(s.remove(k));
        debug assertThrown!RangeError(dummy(s[k]));
    }

    s[K(0)] = V.init;
    auto vp = K(0) in s;
    debug static assert(is(typeof(vp) == V*));

    assert(s.remove(K(0)));
    assert(K(0) !in s);

    X t;
    t.reserveExtra(4096);
}

/// constness inference of ranges
pure nothrow unittest
{
    version(showEntries) dbg();
    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;

    alias K = Nullable!(uint, uint.max);
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = OpenHashMap!(K, V, FNV!(64, true));
    const x = X();

    foreach (ref e; x.byKey)
    {
        debug static assert(is(typeof(e) == const(X.KeyType)));
    }

    foreach (ref e; x.byValue)
    {
        debug static assert(is(typeof(e) == const(X.ValueType)));
    }

    foreach (e; x.byKeyValue)
    {
        debug static assert(is(typeof(e.key) == const(X.KeyType)));
        debug static assert(is(typeof(e.value) == const(X.ValueType)));
        debug static assert(is(typeof(e) == const(X.ElementType)));
    }
}

/// range key constness and value mutability with `class` value
pure nothrow unittest
{
    version(showEntries) dbg();
    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;

    struct S
    {
        uint value;
    }
    alias K = Nullable!(S, S(uint.min)); // use uint.min to trigger use of faster `Allocator.allocateZeroed`

    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = OpenHashMap!(K, V, FNV!(64, true));
    auto x = X();

    x[K(S(42))] = new V(43);

    assert(x.length == 1);

    foreach (e; x.byValue)      // `e` is auto ref
    {
        debug static assert(is(typeof(e) == X.ValueType)); // mutable access to value
        assert(e.data == 43);

        // value mutation side effects
        e.data += 1;
        assert(e.data == 44);
        e.data -= 1;
        assert(e.data == 43);
    }

    foreach (ref e; x.byKeyValue)   // `e` is auto ref
    {
        debug static assert(is(typeof(e.key) == const(X.KeyType))); // const access to key
        debug static assert(is(typeof(e.value) == X.ValueType)); // mutable access to value

        assert(e.key.value == 42);
        assert(e.value.data == 43);

        // key cannot be mutated
        debug static assert(!__traits(compiles, { e.key.value += 1; }));

        // value mutation side effects
        e.value.data += 1;
        assert(e.value.data == 44);
        e.value.data -= 1;
        assert(e.value.data == 43);
    }
}

/// range key constness and value mutability with `class` key and `class` value
pure nothrow unittest
{
    import nxt.digestx.fnv : FNV;

    version(showEntries) dbg();
    class K
    {
        this(uint value)
        {
            this.value = value;
        }

        @property bool opEquals(const scope typeof(this) rhs) const
        {
            return value == rhs.value;
        }

        uint value;
    }

    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = OpenHashMap!(K, V, FNV!(64, true));
    auto x = X();

    x[new K(42)] = new V(43);

    assert(x.length == 1);

    foreach (e; x.byValue)      // `e` is auto ref
    {
        debug static assert(is(typeof(e) == X.ValueType)); // mutable access to value
        assert(e.data == 43);

        // value mutation side effects
        e.data += 1;
        assert(e.data == 44);
        e.data -= 1;
        assert(e.data == 43);
    }

    foreach (ref e; x.byKeyValue)   // `e` is auto ref
    {
        debug static assert(is(typeof(e.key) == X.KeyType)); // mutable access to class key
        debug static assert(is(typeof(e.value) == X.ValueType)); // mutable access to value

        assert(e.key.value == 42);
        assert(e.value.data == 43);

        // class key itself should not be mutable
        debug static assert(!__traits(compiles, { e.key = null; }));

        // members of key can be mutated
        debug static assert(__traits(compiles, { e.key.value += 1; }));

        // value mutation side effects
        e.value.data += 1;
        assert(e.value.data == 44);
        e.value.data -= 1;
        assert(e.value.data == 43);
    }
}

/// range key constness and value mutability with `class` key and `class` value
pure nothrow unittest
{
    import nxt.digestx.fnv : FNV;
    version(showEntries) dbg();
    class K
    {
        this(uint value)
        {
            this.value = value;
        }
        uint value;
    }

    struct V
    {
        this(uint data) { this.data = data; }
        @disable this(this);
        uint data;
    }

    alias X = OpenHashMap!(K, V, FNV!(64, true));
    auto x = X();

    scope key42 = new K(42);
    x[key42] = V(43);

    assert(x.length == 1);

    foreach (ref e; x.byValue)  // `e` is auto ref
    {
        debug static assert(is(typeof(e) == X.ValueType)); // mutable access to value
        assert(e.data == 43);

        // value mutation side effects
        e.data += 1;
        assert(e.data == 44);
        e.data -= 1;
        assert(e.data == 43);
    }

    foreach (ref e; x.byKeyValue) // `e` is auto ref
    {
        debug static assert(is(typeof(e.key) == X.KeyType)); // mutable access to class key
        debug static assert(is(typeof(e.value) == X.ValueType)); // mutable access to value

        assert(e.key.value == 42);
        assert(e.value.data == 43);

        // value mutation side effects
        e.value.data += 1;
        assert(e.value.data == 44);
        e.value.data -= 1;
        assert(e.value.data == 43);
    }

    assert(x.length == 1);

    assert(x.remove(key42));
    assert(x.length == 0);

    x[key42] = V(43);
    assert(x.length == 1);
}

version(unittest)
{
    T make(T)(ulong value)
    {
        static if (is(T == class))
        {
            return new T(value);
        }
        else
        {
            return T(value);
        }
    }
}

/// test various things
@trusted unittest
{
    import std.meta : AliasSeq;
    import std.typecons : Nullable;
    import std.algorithm.comparison : equal;
    import nxt.container_traits : mustAddGCRange;
    import nxt.digestx.fnv : FNV;
    import nxt.array_help : s;

    version(showEntries) dbg();
    const n = 100;

    void testEmptyAll(K, V, X)(ref X x, size_t n,
                               scope K[] keys)
    {
        assert(x.length == n);
        foreach (key; keys)
        {
            static if (X.hasValue)
            {
                const element = X.ElementType(key, V.init);
            }
            else
            {
                alias element = key;
            }

            assert(x.length == n - key.get);

            const hitPtr = key in x;
            static if (X.hasValue)
            {
                assert(hitPtr && *hitPtr is element.value);
            }
            else
            {
                assert(hitPtr && *hitPtr is element);
            }

            assert(x.remove(key));
            assert(x.length == n - key.get - 1);

            static if (!X.hasValue)
            {
                assert(!x.contains(key));
                assert(!x.containsUsingLinearSearch(key));
            }
            assert(key !in x);
            assert(!x.remove(key));
            assert(x.length == n - key.get - 1);
        }

        assert(x.length == 0);

        x.clear();
        assert(x.length == 0);
    }

    X testDup(X)(scope ref X x, size_t n)
    {
        typeof(return) y = x.dup;

        assert(x._store.ptr !is y._store.ptr);
        assert(x.length == y.length);
        assert(y.length == n);
        // non-symmetric algorithm so both are needed
        assert(y == x);
        assert(x == y);

        static if (X.hasValue)
        {
            assert(equal(x.byKey,
                         y.byKey));
            assert(equal(x.byValue,
                         y.byValue));
            assert(equal(x.byKeyValue,
                         y.byKeyValue));
        }
        else
        {
            assert(equal(x.byElement,
                         y.byElement));
        }

        debug static assert(!__traits(compiles, { const _ = x < y; })); // no ordering

        return y;
    }

    alias NullableUlong = Nullable!(ulong, ulong.max);

    static class SomeSimpleClass
    {
        @safe pure nothrow @nogc
        this(ulong value)
        {
            this._value = value;
        }

        @safe pure nothrow @nogc
        ulong get() const
        {
            return _value;
        }

        @property void toString(scope void delegate(scope const(char)[]) sink) const
        {
            import std.format : formattedWrite;
            sink.formattedWrite(typeof(this).stringof, "(%s)", _value);
        }

        @property bool opEquals(const scope typeof(this) rhs) const
        {
            return _value == rhs._value;
        }

        private ulong _value;
    }

    debug static assert(mustAddGCRange!string);

    foreach (K; AliasSeq!(SomeSimpleClass,
                          NullableUlong))
    {
        foreach (V; AliasSeq!(string, int, void))
        {
            alias X = OpenHashMap!(K, V, FNV!(64, true));

            auto k11 = make!K(11);
            auto k12 = make!K(12);
            auto k13 = make!K(13);

            static if (!X.hasValue)
            {
                auto x = X.withElements([k11, k12, k13].s);

                import std.algorithm : count;

                // ByLvalueElement
                auto xr = x.byElement;

                alias R = typeof(xr);
                import std.range.primitives : isInputRange;
                import std.traits : ReturnType;
                debug static assert(is(typeof(R.init) == R));
                debug static assert(is(ReturnType!((R xr) => xr.empty) == bool));

                debug static assert(!__traits(compiles, { xr.front == K.init; })); // always head-const
                auto f = xr.front;
                static if (is(K == class))
                {
                    debug static assert(is(typeof(f) == K)); // tail-mutable
                }
                else
                {
                    debug static assert(is(typeof(f) == const(K))); // tail-const
                }

                debug static assert(is(typeof((R xr) => xr.front)));
                debug static assert(!is(ReturnType!((R xr) => xr.front) == void));
                debug static assert(is(typeof((R xr) => xr.popFront)));

                debug static assert(isInputRange!(typeof(xr)));

                assert(x.byElement.count == 3);

                X y;
                size_t ix = 0;
                foreach (ref e; x.byElement)
                {
                    assert(x.contains(e));
                    assert(x.containsUsingLinearSearch(e));
                    assert(!y.contains(e));
                    assert(!y.containsUsingLinearSearch(e));
                    static if (is(K == class))
                    {
                        y.insert(cast(K)e); // ugly but ok in tests
                    }
                    else
                    {
                        y.insert(e);
                    }
                    assert(y.contains(e));
                    assert(y.containsUsingLinearSearch(e));
                    ix++;
                }

                assert(y.byElement.count == 3);
                assert(x == y);

                const z = X();
                assert(z.byElement.count == 0);

                immutable w = X();
                assert(w.byElement.count == 0);

                {
                    auto xc = X.withElements([k11, k12, k13].s);
                    assert(xc.length == 3);
                    assert(xc.contains(k11));
                    assert(xc.containsUsingLinearSearch(k11));

                    // TODO: http://forum.dlang.org/post/kvwrktmameivubnaifdx@forum.dlang.org
                    xc.removeAllMatching!(_ => _ == k11);

                    assert(xc.length == 2);
                    assert(!xc.contains(k11));
                    assert(!xc.containsUsingLinearSearch(k11));

                    xc.removeAllMatching!(_ => _ == k12);
                    assert(!xc.contains(k12));
                    assert(!xc.containsUsingLinearSearch(k12));
                    assert(xc.length == 1);

                    xc.removeAllMatching!(_ => _ == k13);
                    assert(!xc.contains(k13));
                    assert(!xc.containsUsingLinearSearch(k13));
                    assert(xc.length == 0);

                    // this is ok
                    foreach (e; xc.byElement) {}
                }

                {               // ByRvalueElement
                    auto k = X.withElements([k11, k12].s).filtered!(_ => _ != k11).byElement;
                    debug static assert(isInputRange!(typeof(k)));
                    assert(k.front == k12);

                    debug static assert(!__traits(compiles, { k.front = K.init; })); // head-const
                    static if (is(K == class))
                    {
                        debug static assert(is(typeof(k.front) == K)); // tail-mutable
                    }
                    else
                    {
                        debug static assert(is(typeof(k.front) == const(K))); // tail-const
                    }

                    k.popFront();
                    assert(k.empty);
                }

                {
                    X q;
                    auto qv = [make!K(11U), make!K(12U), make!K(13U), make!K(14U)].s;
                    q.insertN(qv[]);
                    foreach (e; qv[])
                    {
                        assert(q.contains(e));
                        assert(q.containsUsingLinearSearch(e));
                    }
                    q.clear();
                    assert(q.empty);
                }
            }

            static if (is(V == string))
            {
                debug static assert(mustAddGCRange!V);
                debug static assert(mustAddGCRange!(V[1]));
                debug static assert(mustAddGCRange!(X.T));
            }

            auto x1 = X();            // start empty

            // fill x1

            import std.array : Appender;
            Appender!(K[]) keys;

            foreach (immutable key_; 0 .. n)
            {
                auto key = make!K(key_);
                keys.put(key);

                // create elements
                static if (X.hasValue)
                {
                    auto value = V.init;
                    auto element = X.ElementType(key, value);
                }
                else
                {
                    // no assignment because Nullable.opAssign may leave rhs in null state
                    auto element = key;
                }

                assert(key !in x1);

                assert(x1.length == key.get);
                assert(x1.insert(element) == X.InsertionStatus.added);
                assert(x1.length == key.get + 1);

                static if (X.hasValue)
                {
                    import std.conv : to;
                    auto e2 = X.ElementType(key, (42 + key_).to!V);
                    assert(x1.insert(e2) == X.InsertionStatus.modified);
                    assert(x1.contains(key));
                    assert(x1.get(key, V.init) == (42 + key_).to!V);

                    assert(x1.remove(key));
                    assert(!x1.contains(key));

                    x1[key] = value; // restore value
                    assert(x1.contains(key));
                }

                assert(x1.length == key.get + 1);

                const hitPtr = key in x1;
                static if (X.hasValue)
                {
                    assert(hitPtr && *hitPtr == value);
                }
                else
                {
                    assert(hitPtr && *hitPtr is key);
                }

                auto status = x1.insert(element);
                assert(status == X.InsertionStatus.unmodified);
                static if (X.hasValue)
                {
                    assert(x1.insert(key, value) == X.InsertionStatus.unmodified);
                }
                assert(x1.length == key.get + 1);

                assert(key in x1);
            }

            static if (X.hasValue)
            {
                import nxt.dynamic_array : Array = DynamicArray;
                Array!(X.ElementType) a1; // remember the keys

                foreach (const ref key; x1.byKey)
                {
                    auto keyPtr = key in x1;
                    assert(keyPtr);
                    a1 ~= X.ElementType(cast(K)key, (*keyPtr));
                }

                assert(x1.length == a1.length);

                foreach (ae; a1[])
                {
                    auto keyPtr = ae.key in x1;
                    assert(keyPtr);
                    assert((*keyPtr) is ae.value);
                }
            }

            assert(x1.length == n);

            auto x2 = testDup(x1, n);

            testEmptyAll!(K, V)(x1, n, keys.data);

            testEmptyAll!(K, V)(x2, n, keys.data); // should be not affected by emptying of x1
        }
    }
}

///
@safe pure nothrow @nogc unittest
{
    version(showEntries) dbg();
    import std.typecons : Nullable;
    import nxt.digestx.fnv : FNV;

    alias X = OpenHashMap!(Nullable!(size_t, size_t.max), size_t, FNV!(64, true));
    import nxt.dynamic_array : Array = DynamicArray;
    X x;
    // TODO: these segfault:
    // TODO: auto a = Array!(X.KeyType).withElementsOfRange_untested(x.byKey); // l-value byKey
    // TODO: auto b = Array!(X.KeyType).withElementsOfRange_untested(X().byKey); // r-value byKey
}

/// manual Nullable type
@safe pure unittest
{
    import nxt.digestx.fnv : FNV;

    static class Zing
    {
        @safe pure nothrow @nogc:
        this(ulong value) { this._value = value; }
        private ulong _value;
    }
    debug static assert(isNullable!Zing);

    enum Alt
    {
        unknown,
        a,
        b,
        c,
        d
    }

    struct ZingRelation
    {
        Zing zing;
        Alt alts;

        alias nullifier = zing;
        static immutable nullValue = typeof(this).init;

        bool opEquals(const scope typeof(this) that) const @safe pure nothrow @nogc
        {
            return (this.zing is that.zing &&
                    this.alts == that.alts);
        }
    }
    debug static assert(isNullable!ZingRelation);

    alias X = OpenHashSet!(ZingRelation, FNV!(64, true));
    debug static assert(X.sizeof == 32); // TODO: fix hole handling and change to 24
    X x;

    scope e = ZingRelation(new Zing(42), Alt.init);

    assert(!x.contains(e));
    assert(!x.containsUsingLinearSearch(e));
    assert(x.insert(e) == X.InsertionStatus.added);
    assert(x.contains(e));
    assert(x.containsUsingLinearSearch(e));
}

/// abstract class value type
@safe unittest
{
    static abstract class Zing
    {
        @safe pure nothrow @nogc:
    }
    static class Node : Zing
    {
        @safe pure nothrow @nogc:
    }

    alias X = OpenHashSet!(Zing);
    X x;

    const Zing cz = new Node();
    x.insert(cz);               // ok to insert const

    Zing z = new Node();
    x.insert(z); // ok to insert mutable because hashing is on address by default
}

/// class type with default hashing
@safe unittest
{
    static class Base
    {
        static size_t dtorCount = 0; // number of calls to this destructor
    @safe nothrow @nogc:
        ~this() @nogc { dtorCount += 1; }
    pure:
        this(ulong value) { this._value = value; }
        @property bool opEquals(const scope typeof(this) rhs) const
        {
            return _value == rhs._value;
        }
        override hash_t toHash() const
        {
            return hashOf(_value);
        }
        private ulong _value;
    }

    /** Node containing same data members but different type. */
    static class Node : Base
    {
        @safe pure nothrow @nogc:
        this(ulong value) { super(value);  }
    }
    debug static assert(is(Node : Base));

    import nxt.hash_functions : hashOfPolymorphic; // neede to separate hash of `Base(N)` from `Node(N)`
    alias X = OpenHashSet!(Base, hashOfPolymorphic, "a && b && (typeid(a) is typeid(b)) && a.opEquals(b)");
    debug static assert(X.sizeof == 24);
    X x;

    // top-class
    scope b42 = new Base(42);
    assert(!x.contains(b42));
    assert(!x.containsUsingLinearSearch(b42));
    assert(x.insert(b42) == X.InsertionStatus.added);
    assert(x.contains(b42));
    assert(x.containsUsingLinearSearch(b42));
    assert(x.tryGetElementFromCtorParams!Base(42) !is null);
    assert(Base.dtorCount == 1);
    assert(x.tryGetElementFromCtorParams!Base(42)._value == 42);
    assert(Base.dtorCount == 2);
    assert(x.tryGetElementFromCtorParams!Base(41) is null);
    assert(Base.dtorCount == 3);

    // top-class
    scope b43 = new Base(43);
    assert(!x.contains(b43));
    assert(!x.containsUsingLinearSearch(b43));
    assert(x.insert(b43) == X.InsertionStatus.added);
    assert(x.contains(b43));
    assert(x.containsUsingLinearSearch(b43));
    assert(x.tryGetElementFromCtorParams!Base(43) !is null);
    assert(Base.dtorCount == 4);
    assert(x.tryGetElementFromCtorParams!Base(43)._value == 43);
    assert(Base.dtorCount == 5);

    // sub-class
    assert(x.tryGetElementFromCtorParams!Node(42) is null);
    assert(Base.dtorCount == 6);
    immutable n42 = new Node(42);
    assert(!x.contains(n42));     // mustn't equal to `b42`
    assert(!x.containsUsingLinearSearch(n42)); // mustn't equal to `b42`
    assert(x.insert(n42) == X.InsertionStatus.added); // added as separate type
    assert(x.contains(n42));
    assert(x.containsUsingLinearSearch(n42));
    assert(x.tryGetElementFromCtorParams!Node(42) !is null);
    assert(Base.dtorCount == 7);
    assert(x.tryGetElementFromCtorParams!Node(42)._value == 42);
    assert(Base.dtorCount == 8);

    assert(hashOf(b42) == hashOf(n42));

    // sub-class
    assert(x.tryGetElementFromCtorParams!Node(43) is null);
    assert(Base.dtorCount == 9);
    auto n43 = new Node(43);
    assert(!x.contains(n43));     // mustn't equal to `b43`
    assert(!x.containsUsingLinearSearch(n43)); // mustn't equal to `b43`
    assert(x.insert(n43) == X.InsertionStatus.added); // added as separate type
    assert(x.contains(n43));
    assert(x.containsUsingLinearSearch(n43));
    assert(x.tryGetElementFromCtorParams!Node(43) !is null);
    assert(Base.dtorCount == 10);
    assert(x.tryGetElementFromCtorParams!Node(43)._value == 43);
    assert(Base.dtorCount == 11);

    assert(hashOf(b43) == hashOf(n43));
}

/// enumeration key
@safe pure unittest
{
    import nxt.digestx.fnv : FNV;

    enum Alt
    {
        nullValue,              // trait
        a, b, c, d
    }
    alias X = OpenHashSet!(Alt, FNV!(64, true));
    X x;
    assert(!x.contains(Alt.a));

    assert(x.insert(Alt.a) == X.InsertionStatus.added);

    assert(x.contains(Alt.a));
    assert(x.containsUsingLinearSearch(Alt.a));
    assert(!x.contains(Alt.b));
    assert(!x.contains(Alt.c));
    assert(!x.contains(Alt.d));
    assert(!x.containsUsingLinearSearch(Alt.b));
    assert(!x.containsUsingLinearSearch(Alt.c));
    assert(!x.containsUsingLinearSearch(Alt.d));

    assert(x.remove(Alt.a));
    assert(!x.contains(Alt.a));
    assert(!x.containsUsingLinearSearch(Alt.a));
}

///
@safe pure nothrow
unittest
{
    import nxt.digestx.fnv : FNV;
    static struct Rel
    {
        static immutable nullValue = typeof(this).init;
        string name;            // relation name. WARNING compiler crashes when qualified with `package`
    }
    alias X = OpenHashSet!(Rel, FNV!(64, true));
    X x;
    foreach (const i; 0 .. 100)
    {
        const char[1] ch = ['a' + i];
        assert(!x.contains(Rel(ch.idup)));
        assert(!x.containsUsingLinearSearch(Rel(ch.idup)));
        x.insert(Rel(ch.idup));
        assert(x.contains(Rel(ch.idup)));
        /* TODO: assert(x.containsUsingLinearSearch(Rel(ch.idup))); */
    }
}

/// `SSOString` as set key type
@safe pure nothrow @nogc
unittest
{
    import nxt.sso_string : SSOString;
    import nxt.digestx.fnv : FNV;

    alias K = SSOString;
    static assert(isHoleable!K);
    alias X = OpenHashSet!(K, FNV!(64, true));
    const n = 100;

    X a;
    foreach (const i; 0 .. n)
    {
        const char[1] ch = ['a' + i];
        const k = K(ch);        // @nogc

        assert(!a.contains(k));
        assert(!a.containsUsingLinearSearch(k));

        assert(a.insert(K(ch)) == X.InsertionStatus.added);
        // TODO: assert(a.insertAndReturnElement(K(ch)) == k);
        assert(a.contains(k));
        assert(a.containsUsingLinearSearch(k));

        assert(a.remove(k));
        assert(!a.contains(k));
        assert(a.insert(K(ch)) == X.InsertionStatus.added);

        assert(a.remove(ch[]));
        assert(!a.contains(k));
        assert(a.insert(K(ch)) == X.InsertionStatus.added);
    }

    X b;
    foreach (const i; 0 .. n)
    {
        const char[1] ch = ['a' + (n - 1 - i)];
        const k = K(ch);        // @nogc

        assert(!b.contains(k));
        assert(!b.containsUsingLinearSearch(k));

        assert(b.insert(K(ch)) == X.InsertionStatus.added);
        // TODO: assert(b.insertAndReturnElement(K(ch)) == k);

        assert(b.contains(k));
        assert(b.containsUsingLinearSearch(k));

        assert(b.remove(k));
        assert(!b.contains(k));

        assert(b.insert(K(ch)) == X.InsertionStatus.added);
    }

    assert(a == b);

    const us = K("_");
    assert(!a.contains(us));
    a ~= us;
    assert(a.contains(us));
}

/// test `opIndexOpAssign`
@safe pure nothrow
unittest
{
    import nxt.sso_string : SSOString;
    import nxt.digestx.fnv : FNV;

    alias K = SSOString;
    alias V = long;
    alias X = OpenHashMap!(K, V, FNV!(64, true));

    X x;

    const a = K("a");
    const b = K("b");

    x[a] = 17;
    assert(x[a] == 17);

    x[a] += 10;                 // opIndexOpAssign!("+=") with existing key
    assert(x[a] == 27);

    x[b] += 10;                 // opIndexOpAssign!("+=") with non-existing key
    assert(x[b] == 10);

    x[b] *= 10;                 // opIndexOpAssign!("*=") with non-existing key
    assert(x[b] == 100);

    assert(x.length == 2);

    assert(x.contains(a));
    assert(x.contains(a[]));
    assert(a in x);
    assert(a[] in x);

    assert(x.contains(b));
    assert(x.contains(b[]));
    assert(b in x);
    assert(b[] in x);

    const c = K("c");
    assert(!x.contains(c));
    assert(!x.contains(c[]));
    assert(c !in x);
    assert(c[] !in x);
}

/// use prime numbers as capacity
@safe pure unittest
{
    import nxt.address : Address;
    alias K = Address;
    alias V = size_t;
    enum bool usePrimeCapacity = false; // TODO: enable
    alias M = OpenHashMap!(Address, V,
                           hashOf,
                           defaultKeyEqualPredOf!K,
                           Mallocator.instance,
                           false,
                           true,
                           usePrimeCapacity);
    M x;
}

/// `SSOString` as map key type
@safe pure nothrow @nogc
unittest
{
    import nxt.sso_string : SSOString;
    import nxt.digestx.fnv : FNV;
    alias K = SSOString;
    alias V = long;
    alias X = OpenHashMap!(K, V, FNV!(64, true));
    const n = 100;

    immutable default_k = K("miss");

    X a;

    // insert all
    foreach (const i; 0 .. n)
    {
        const char[1] ch = ['a' + i];
        const k = K(ch);        // @nogc
        assert(k[] == ch[]);

        assert(!a.contains(k));
        assert(!a.contains(ch[]));                          // @nogc
        assert(a.getKeyRef(k, default_k)[] is default_k[]); // on miss use `default_k`
        // TODO: assert(a.getKeyRef(ch, default_k)[] is default_k[]); // on miss use `default_k`

        a[k] = V.init;

        assert(a.contains(k));
        assert(a.contains(ch[]));                    // @nogc
        assert(a.getKeyRef(k, default_k)[] !is k[]); // on hit doesn't use `default_k`
        assert(a.getKeyRef(k, default_k)[] == ch);
        // TODO: assert(a.getKeyRef(ch, default_k)[] !is k[]); // on hit doesn't use `default_k`
        // assert(a.getKeyRef(ch, default_k)[] == ch);
    }
    assert(a.length == n);

    // remove all
    foreach (const i; 0 .. n)
    {
        const char[1] ch = ['a' + i];
        const k = K(ch);        // @nogc
        assert(a.contains(k));
        assert(a.remove(k));
        assert(!a.contains(k));
    }
    assert(a.length == 0);

    // insert all again
    foreach (const i; 0 .. n)
    {
        const char[1] ch = ['a' + i];
        const k = K(ch);        // @nogc
        assert(k[] == ch[]);

        assert(!a.contains(k));
        assert(!a.contains(ch[]));                          // @nogc
        assert(a.getKeyRef(k, default_k)[] is default_k[]); // on miss use `default_k`
        // TODO: assert(a.getKeyRef(ch, default_k)[] is default_k[]); // on miss use `default_k`

        a[k] = V.init;
    }
    assert(a.length == n);

    X b;
    foreach (const i; 0 .. n)
    {
        const char[1] ch = ['a' + (n - 1 - i)];
        const k = K(ch);        // @nogc

        assert(!b.contains(k));

        b[k] = V.init;

        assert(b.contains(k));
    }

    assert(a == b);
}

/** Is `true` iff `T` is a memory address (either a `class` or a pointer). */
enum bool isAddress(T) = (is(T == class) ||
                          (is(T == U*, U) &&
                           // exclude alias this:
                           !(is(T == struct) ||
                             is(T == union) ||
                             is(T == interface))));

/** Is `true` iff `T` has a specific value dedicated to representing holes
 * (removed/erase) values.
 */
enum isHoleable(T) = (// __traits(hasMember, T, "isHole") &&
                      // __traits(hasMember, T, "holeify") &&
    __traits(hasMember, T, "holeValue"));

/** Default key equality/equivalence predicate for the type `T`.
 */
template defaultKeyEqualPredOf(T)
{
    static if (is(T == class))
    {
        // static assert(__traits(hasMember, T, "opEquals"),
        //               "Type" ~ T.stringof ~ " doesn't have local opEquals() defined");
        // enum defaultKeyEqualPredOf = "a && b && a.opEquals(b)";
        enum defaultKeyEqualPredOf = "a is b";
        // (const T a, const T b) => ((a !is null) && (b !is null) && a.opEquals(b));
    }
    else
    {
        enum defaultKeyEqualPredOf = "a == b";
    }
}

///
@safe pure nothrow unittest
{
    class C
    {
        @safe pure nothrow @nogc:
        this(int x)
        {
            this.x = x;
        }
        @property bool opEquals(const scope typeof(this) rhs) const
        {
            return x == rhs.x;
        }
        @property override bool opEquals(const scope Object rhs) const @trusted
        {
            C rhs_ = cast(C)rhs;
            return rhs_ && x == rhs_.x;
        }
        int x;
    }
    static assert(defaultKeyEqualPredOf!(C) == "a is b");
}

version(showEntries) import nxt.dbgio : dbg;
