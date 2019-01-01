module open_hashmap_or_hashset;

// version = showEntries;
// version = internalUnittest; // fed by dub (see dub.sdl) in unittest-internal mode

import container_traits : isNullable;
import pure_mallocator : Mallocator = PureMallocator;
// TODO import std.experimental.allocator.mallocator : Mallocator;

/** Is `true` iff `T` is a memory address. */
private template isAddress(T)
{
    import std.traits : isPointer;
    enum isAddress = (is(T == class) || // a class is memory-wise
                      isPointer!T);     // just a pointer, consistent with opCmp
}

/** Is `true` iff `T` has a specific value dedicated to representing holes
 * (removed/erase) values.
 */
enum isHoleable(T) = (__traits(hasMember, T, "isHole") &&
                      __traits(hasMember, T, "holeify") &&
                      __traits(hasMember, T, "holeValue"));

private template defaultKeyEqualPredOf(T)
{
    static if (is(T == class))
    {
        static assert(__traits(hasMember, T, "opEquals"),
                      "Type" ~ T.stringof ~ " doesn't have local opEquals() defined");
        // enum defaultKeyEqualPredOf = "a && b && a.opEquals(b)";
        enum defaultKeyEqualPredOf = "a is b";
        // (const T a, const T b) => ((a !is null) && (b !is null) && a.opEquals(b));
    }
    else
    {
        enum defaultKeyEqualPredOf = "a == b";
    }
    version(none)
    {
        import std.traits : isArray, isCopyable;
        static if (isArray!T)
        {
            // compare arrays by elements only, regardless of location
            enum defaultKeyEqualPredOf = "a == b";
            /* alias defaultKeyEqualPredOf = (const scope a, */
            /*                       const scope b) => a == b; */
        }
        else static if (isCopyable!T)
        {
            enum defaultKeyEqualPredOf = "a is b";
            /* alias defaultKeyEqualPredOf = (const scope a, */
            /*                       const scope b) => a is b; */
        }
        else
        {
            enum defaultKeyEqualPredOf = "a is b";
            /* alias defaultKeyEqualPredOf = (const scope ref a, */
            /*                       const scope ref b) => a is b; */
        }
    }
}

/** Returns `true` iff `lhs` and `rhs` are equal.
 *
 * Opposite to druntime version, implementation is parameterized on object type
 * `T` enabling correct propagation of function qualifiers of `lhs.opEquals(rhs)`.
 */
bool opEqualsDerived(T)(const T lhs, const T rhs)
if (is(T == class))
{
    // If aliased to the same object or both null => equal
    if (lhs is rhs) return true;

    // If either is null => non-equal
    if (lhs is null || rhs is null) return false;

    return lhs.opEquals(rhs);

    version(none)
    {
        // If same exact type => one call to method opEquals
        if (typeid(lhs) is typeid(rhs)//  ||
            // TODO !__ctfe && typeid(lhs).opEquals(typeid(rhs))
            )
            /* CTFE doesn't like typeid much. 'is' works, but opEquals doesn't
               (issue 7147). But CTFE also guarantees that equal TypeInfos are
               always identical. So, no opEquals needed during CTFE. */
        {
            return lhs.opEquals(rhs);
        }

        // General case => symmetric calls to method opEquals
        return lhs.opEquals(rhs) && rhs.opEquals(lhs);
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
    assert( opEqualsDerived(new C(42), new C(42)));
    assert(!opEqualsDerived(new C(42), new C(43)));
}

@safe:

/** Hash table/map (or set) with open-addressing, storing (key) elements of type
 * `K` and values of type `V`.
 *
 * Keys are immutable except for when they are `class`es in which case they are
 * head-const (through bin reinterpretation to `KeyValueType`), This corresponds
 * with behaviour of default value hashing of `class` instances in
 * `digestion.d`.
 *
 * Uses open-addressing with quadratic probing (using triangular numbers) and
 * lazy deletion/removal.
 *
 * Params:
 *      K = key type.
 *      V = value type.
 *      hasher = hash function or std.digest Hash.
 *      Allocator = memory allocator for bin array
 *      borrowChecked = only activate when it's certain that this won't be moved via std.algorithm.mutation.move()
 *
 * See_Also: https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/
 * See_Also: https://en.wikipedia.org/wiki/Lazy_deletion
 * See_Also: https://forum.dlang.org/post/ejqhcsvdyyqtntkgzgae@forum.dlang.org
 *
 * TODO check that hole value is not used alongside the check assert(!key.isNull)
 *
 * TODO add support for checking existence `K.nullifier` that infers, for
 * instance, how to tag a `ZingRel` and `Expr` as `null` or a `hole`.
 *
 * TODO allocate _holesPtr array together with _bins to reduce size of
 * `OpenHashMapOrSet` to 3 words when element type doesn't support it
 *
 * TODO modify existing unittest for `struct Rel { const string name; }`
 *
 * TODO use allocator.dispose() instead of allocator.deallocate() as in
 * https://github.com/dlang-community/containers
 *
 * TODO if hash-function is cast(size_t)(classInstance) always use prime length
 * and shift pointer before hash based on alignof (might not be needed when
 * module prime) to maximize memory locality when adding successively allocated
 * pointers
 *
 * TODO fix bug in `growInPlaceWithCapacity` and benchmark
 *
 * TODO add extractElement that moves it out similar to
 * http://en.cppreference.com/w/cpp/container/unordered_set/extract
 *
 * TODO add merge or union algorithm here or into container_algorithm.d. See
 * also: http://en.cppreference.com/w/cpp/container/unordered_set/merge. this
 * algorithm moves elements from source if they are not already in `this`
 *
 * TODO Robin-Hood-hashing
 *
 * TODO enable `borrowChecked` unconditionally in version(debug) if and when
 * `opMove` is implemented, in which case opMove() should assert false if this
 * is borrowed. See: https://github.com/dlang/DIPs/pull/109
 *
 * TODO keep only predicates with ref arguments when LDC can optimize those as
 * fast as value passing. add LDC issue for this
 *
 * TODO always use `const scope auto ref` in predicates (including when
 * `isCopyable!K` is true) to reduce static if branch complexity
 */
struct OpenHashMapOrSet(K, V = void,
                        alias hasher = hashOf,
                        string keyEqualPred = defaultKeyEqualPredOf!(K),
                        alias Allocator = Mallocator.instance,
                        bool borrowChecked = false)
    if (isNullable!K
        // isHashable!K
        )
{
    // pragma(msg, K.stringof, " => ", V.stringof);
    import core.exception : onOutOfMemoryError;
    import std.algorithm.mutation : move, moveEmplace;
    import std.conv : emplace;
    import std.math : nextPow2;
    import std.traits : hasElaborateDestructor, isCopyable, hasIndirections,
        isDynamicArray, isStaticArray, Unqual, hasFunctionAttributes, isMutable, TemplateArgsOf;
    import std.typecons : Nullable;

    import container_traits : defaultNullKeyConstantOf, mustAddGCRange, isNull, nullify;
    import qcmeman : gc_addRange, gc_removeRange;
    import probing : triangularProbeFromIndex, triangularProbeFromIndexIncludingHoles, triangularProbeCountFromIndex;

    import std.functional : binaryFun;
    alias keyEqualPredFn = binaryFun!keyEqualPred;

    enum isBorrowChecked = borrowChecked;

    /** In the hash map case, `V` is non-void, and a value is stored alongside
     * the key of type `K`.
     */
    enum hasValue = !is(V == void);

    /** Is `true` iff `K` is an address, in which case holes are represented by
     * a specific value `holeKeyConstant`.
     */
    enum hasAddressLikeKey = (isAddress!K ||
                              isDynamicArray!K);

    /** Is `true` iff `K` has a specific value `holeKeyConstant` that represent
     * holes.
     */
    enum hasHoleableKey = isHoleable!K || hasAddressLikeKey;

    static if (isHoleable!K)
    {
        static K holeKeyConstant() @safe pure nothrow @nogc
        {
            pragma(inline, true);
            return K.holeValue;
        }
        static bool isHoleKeyConstant(const scope K key) @safe pure nothrow @nogc
        {
            pragma(inline, true);
            return key.isHole;
        }
    }
    else static if (hasAddressLikeKey)
    {
        enum holeKeyOffset = 0x1; // TODO is this a good value?
        enum holeKeyAddress = cast(void*)holeKeyOffset;

        /**
         * See_Also: https://forum.dlang.org/post/p7726n$2apd$1@digitalmars.com
         * TODO test if ulong.max gives better performance
         */
        static K holeKeyConstant() @trusted pure nothrow @nogc
        {
            pragma(inline, true);
            // TODO note that cast(size_t*) will give address 0x8 instead of 0x1
            static if (isDynamicArray!K)
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
            pragma(inline, true);
            static if (isDynamicArray!K) // for slices
            {
                // suffice to compare pointer part
                return (key.ptr is holeKeyAddress);
            }
            else
            {
                return (cast(const(void)*)key is holeKeyAddress);
            }
        }

        /** TODO make these work
         */
        // enum K holeKey_1 = cast(K)((cast(size_t*)null));
        // static immutable K holeKey_2 = cast(K)((cast(size_t*)null));
    }
    else
    {
        static if (__traits(hasMember, K, "nullifier"))
        {
            pragma(msg, "Need explicit hole bitset for non-address-like key: ", K, " with nullifier ", K.nullifier.stringof);
        }
        else
        {
            pragma(msg, "Need explicit hole bitset for non-address-like key: ", K);
        }
        import core.bitop : bts, bt, btr;
        import array_help : makeUninitializedBitArray, makeZeroedBitArray, makeReallocatedBitArrayZeroPadded;
    }

    /// Element type.
    static if (hasValue)
    {
        /** Map insertion status.
         */
        enum InsertionStatus
        {
            added,                      // element was added
            modified,                   // value of element was changed (map only). TODO only for Map-case
            unmodified                  // element was left unchanged
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
            pragma(inline, true);
            return element.key;
        }

        /// Get value part of element.
        static auto ref inout(V) valueOf()(auto ref return scope inout(T) element)
        {
            pragma(inline, true);
            return element.value;
        }

        /** Type of key stored. */
        alias KeyType = K;

        /** Type of value stored. */
        alias ValueType = V;

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
            {
                const K key;
            }
            V value;
        }

        /// Get key part.
        static auto ref inout(K) keyOf()(auto ref return scope inout(KeyValueType) element) @trusted
        {
            pragma(inline, true);
            return cast(typeof(return))element.key; // needed for case: `inout(const(K)) => inout(K)`
        }
    }
    else                        // HashSet
    {
        /** Set insertion status.
         */
        enum InsertionStatus
        {
            added,                      // element was added
            unmodified                  // element was left unchanged
        }

        alias T = K;            // short name for element type

        /// Get key part of element.
        static auto ref inout(SomeElement) keyOf(SomeElement)(auto ref return inout(SomeElement) element)
        {
            pragma(inline, true);
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
        {
            enum isScopedKeyType = (is(const(SomeKey) : const(K)));
        }
        else
        {
            enum isScopedKeyType = (is(K : SomeKey) || // `K is` implicitly convertible from `SomeKey`
                                    is(SomeKey : U[], U) && // is array
                                    is(typeof(K(SomeKey.init))));
        }
    }

    alias ElementType = T;

    /** Make with room for storing at least `minimumCapacity` number of elements.
     *
     * See_Also:
     * https://forum.dlang.org/post/nyngzsaeqxzzuumivtze@forum.dlang.org
     */
    static typeof(this) withCapacity()(size_t minimumCapacity) // template-lazy
    {
        version(LDC) pragma(inline, true);
        version(showEntries) dln(__FUNCTION__, " minimumCapacity:", minimumCapacity);
        return typeof(return)(makeDefaultInitializedBins(minimumCapacity), 0);
    }

    /** Make default-initialized bins with room for storing for at least
     * `minimumCapacity` number of elements.
     */
    static private T[] makeDefaultInitializedBins()(size_t minimumCapacity) // template-lazy
        @trusted pure nothrow @nogc
    {
        version(LDC) pragma(inline, true);
        immutable capacity = nextPow2(minimumCapacity);
        version(showEntries) dln(__FUNCTION__, " minimumCapacity:", minimumCapacity,
                                 " capacity:", capacity);

        // TODO cannot use makeArray here because it cannot handle uncopyable types
        // import std.experimental.allocator : makeArray;
        // auto bins = Allocator.makeArray!T(capacity, nullKeyElement);

        import bit_traits : isAllZeroBits;

        immutable byteCount = T.sizeof*capacity;

        static if (hasAddressLikeKey ||
                   (__traits(isZeroInit, K)  &&
                    __traits(hasMember, K, "nullifier")) ||
                   // TODO add check for __traits(isZeroInit, K) and member `K.nullValue` == `K.init`
                   (__traits(hasMember, K, `nullValue`) && // if key has a null value
                    __traits(compiles, { enum _ = isAllZeroBits!(K, K.nullValue); }) && // prevent strange error given when `K` is `knet.data.Data`
                    isAllZeroBits!(K, K.nullValue))) // check that it's zero bits only
        {
            // pragma(msg, "zero-allocate:", "K:", K, " V:", V);
            // TODO use std.experimental.allocator.makeArray instead of this which handles clever checking for isZeroInit
            import container_traits : makeInitZeroArray;
            auto bins = makeInitZeroArray!(T, Allocator)(capacity);
            if (bins.ptr is null && capacity >= 1)
            {
                onOutOfMemoryError();
            }
        }
        else                    // when default null key is not represented by zeros
        {
            // pragma(msg, "emplace:", "K:", K, " V:", V);
            auto bins = cast(T[])Allocator.allocate(byteCount);
            if (bins.ptr is null && byteCount >= 1)
            {
                onOutOfMemoryError();
            }
            foreach (ref bin; bins)
            {
                enum hasNullValueKey = __traits(hasMember, K, `nullValue`);
                static if (hasNullValueKey &&
                           !is(typeof(emplace(&keyOf(bin), K.nullValue)))) // __traits(compiles) fails here when building knet
                {
                    pragma(msg, __FILE__, ":", __LINE__, ":warning: emplace fails for null-Value key type ", K);
                }
                static if (hasNullValueKey &&
                           is(typeof(emplace(&keyOf(bin), K.nullValue)))) // __traits(compiles) fails here when building knet
                {
                    emplace(&keyOf(bin), K.nullValue);
                }
                else
                {
                    emplace(&keyOf(bin));
                    keyOf(bin).nullify(); // moveEmplace doesn't init source of type Nullable
                }
                static if (hasValue)
                {
                    // construct in-place
                    emplace(&valueOf(bin));
                }
            }
        }

        static if (mustAddGCRange!T)
        {
            gc_addRange(bins.ptr, byteCount);
        }

        return bins;
    }

    static private T[] allocateUninitializedBins()(size_t capacity) // template-lazy
        @trusted pure nothrow @nogc
    {
        version(LDC) pragma(inline, true);
        version(showEntries) dln(__FUNCTION__, " newCapacity:", capacity);
        immutable byteCount = T.sizeof*capacity;
        auto bins = cast(typeof(return))Allocator.allocate(byteCount);
        static if (mustAddGCRange!T)
        {
            gc_addRange(bins.ptr, byteCount);
        }
        if (bins.ptr is null && byteCount >= 1)
        {
            onOutOfMemoryError();
        }
        return bins;
    }

    import std.range : StdElementType = ElementType;
    import std.traits : isIterable, isAssignable;

    /** Make with `elements`. */
    static typeof(this) withElements(R)(R elements)
        if (isIterable!R &&
            isAssignable!(T, StdElementType!R))
    {
        version(showEntries) dln(__FUNCTION__, " length:", elements.length);
        import std.range : hasLength;
        static if (hasLength!R)
        {
            typeof(this) that = withCapacity(elements.length);
            foreach (element; elements)
            {
                size_t hitIndex;
                const instationStatus = that.insertWithoutGrowth(element, hitIndex);
            }
        }
        else
        {
            typeof(this) that;
            foreach (ref element; elements)
            {
                that.insert(element);
            }
        }
        return that;
    }

    /// Destruct.
    ~this()
    {
        version(LDC) pragma(inline, true);
        release();
    }

    /// No copying.
    @disable this(this);

    /// Returns: a shallow duplicate of `this`.
    typeof(this) dup()() const @trusted // template-lazy
    {
        // dln(__FUNCTION__, " this:", &this, " with length ", length);
        version(showEntries) dln(__FUNCTION__, " length:", length);
        T[] binsCopy = allocateUninitializedBins(_bins.length); // unsafe
        foreach (immutable index, ref bin; _bins)
        {
            if (isOccupiedAtIndex(index)) // normal case
            {
                static if (hasValue) // map
                {
                    duplicateEmplace(bin.key, binsCopy[index].key);
                    duplicateEmplace(bin.value, binsCopy[index].value);
                }
                else            // set
                {
                    duplicateEmplace(bin, binsCopy[index]);
                }
            }
            else
            {
                emplace(&binsCopy[index]); // TODO only emplace key and not value
                keyOf(binsCopy[index]).nullify();
            }
        }
        static if (!hasHoleableKey)
        {
            if (_holesPtr)
            {
                immutable wordCount = holesWordCount(_bins.length);

                auto holesPtrCopy = makeUninitializedBitArray!Allocator(_bins.length);
                holesPtrCopy[0 .. wordCount] = _holesPtr[0 .. wordCount]; // TODO use memcpy instead?

                static if (isBorrowChecked)
                {
                    debug
                    {
                        return typeof(return)(binsCopy, _count, 0, holesPtrCopy);
                    }
                    else
                    {
                        return typeof(return)(binsCopy, _count, holesPtrCopy);
                    }
                }
                else
                {
                    return typeof(return)(binsCopy, _count, holesPtrCopy);
                }
            }
        }
        return typeof(return)(binsCopy, _count);
    }

    /// Equality.
    bool opEquals()(const scope auto ref typeof(this) rhs) const
    {
        if (_count != rhs._count) { return false; } // quick discardal
        foreach (immutable index, const ref bin; _bins)
        {
            if (isOccupiedAtIndex(index))
            {
                static if (hasValue)
                {
                    auto valuePtr = bin.key in rhs;
                    if (!valuePtr)
                    {
                        return false;
                    }
                    // TODO make != a parameter that can also be typically !is. TODO ask forum about this
                    if ((*valuePtr) != bin.value)
                    {
                        return false;
                    }
                }
                else
                {
                    if (!rhs.contains(bin))
                    {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    static if (true)
    {
    pragma(inline, true):
    private:

        static if (!hasHoleableKey)
        {
            void deallocateHoles() @trusted
            {
                if (_holesPtr)
                {
                    static if (__traits(hasMember, Allocator, "deallocatePtr"))
                    {
                        Allocator.deallocatePtr(_holesPtr);
                    }
                    else
                    {
                        Allocator.deallocate(_holesPtr[0 .. holesWordCount(_bins.length)]);
                    }
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
                return (binCount / wordBits +
                        (binCount % wordBits ? 1 : 0));
            }

            static size_t binBlockBytes(size_t binCount)
            {
                return wordBytes*holesWordCount(binCount);
            }

            void untagHoleAtIndex(size_t index) @trusted
            {
                version(internalUnittest) assert(index < _bins.length);
                if (_holesPtr !is null)
                {
                    btr(_holesPtr, index);
                }
            }

            static bool hasHoleAtPtrIndex(const scope size_t* holesPtr,
                                          size_t index) @trusted
            {
                return (holesPtr &&
                        bt(holesPtr, index) != 0);
            }
        }

        void tagHoleAtIndex(size_t index) @trusted
        {
            version(internalUnittest) assert(index < _bins.length);
            static if (!hasHoleableKey)
            {
                if (_holesPtr is null) // lazy allocation
                {
                    _holesPtr = makeZeroedBitArray!Allocator(_bins.length);
                }
                bts(_holesPtr, index);
            }
            else
            {
                keyOf(_bins[index]) = holeKeyConstant;
            }
        }

    }

    static if (borrowChecked)
    {
        static immutable borrowedErrorMessage = "cannot mutate this when it's borrowed";
    }

    /// Empty.
    void clear()()              // template-lazy
    {
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        release();
        _bins = typeof(_bins).init;
        static if (!hasHoleableKey)
        {
            _holesPtr = null;
        }
        _count = 0;
    }

    /// Release internal allocations.
    private void release() scope
    {
        releaseBinElements();
        releaseBinsAndHolesSlices();
    }

    /// Release bin elements.
    private void releaseBinElements() scope
    {
        foreach (ref bin; _bins)
        {
            static if (hasElaborateDestructor!T)
            {
                .destroy(bin);
            }
        }
    }

    /// Release bin slice.
    private void releaseBinsAndHolesSlices() scope
    {
        releaseBinsSlice(_bins);
        static if (!hasHoleableKey)
        {
            deallocateHoles();
        }
    }

    static private void releaseBinsSlice(T[] bins) @trusted
    {
        version(showEntries) dln(__FUNCTION__, " bins.ptr:", bins.ptr, " bins.length", bins.length);
        if (bins.ptr is null) { return; } // `gc_removeRange` fails for null input
        static if (mustAddGCRange!T)
        {
            gc_removeRange(bins.ptr); // `gc_removeRange` fails for null input
        }
        static if (__traits(hasMember, Allocator, "deallocatePtr"))
        {
            Allocator.deallocatePtr(bins.ptr);
        }
        else
        {
            Allocator.deallocate(bins);
        }
    }

    private auto adjustKeyType(SomeKey)(const return scope SomeKey key) const @trusted
    {
        pragma(inline, true);            // must be inlined
        static if (is(SomeKey : U[], U)) // is array
        {
            /* because return value is used only temporarily it's ok to cast to
             * `immutable` to prevent GC-allocations in types such ass
             * `sso_string.SSOString` */
            return cast(immutable(typeof(key[0]))[])key;
        }
        else
        {
            return key;
        }
    }

    /** Check if `element` is stored.
     *
     * Parameter `key` may be non-immutable, for instance const(char)[]
     * eventhough key type `K` is `string`.
     *
     * Returns: `true` if element is present, `false` otherwise.
     */
    bool contains(SomeKey)(const scope SomeKey key) const @trusted // template-lazy, `auto ref` here makes things slow
    if (isScopedKeyType!(typeof(key)))
    {
        // pragma(msg, SomeKey.stringof ~ " " ~ K.stringof, " ", is(K : SomeKey), " ", is(SomeKey : K));
        // debug static assert(isScopedKeyType!(typeof(key)), SomeKey.stringof ~ " " ~ K.stringof);
        // pragma(msg, SomeKey);
        version(LDC) pragma(inline, true);
        assert(!key.isNull);
        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(K)adjustKeyType(key)); // cast scoped `key` is @trusted
        return (hitIndex != _bins.length &&
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
    bool containsUsingLinearSearch(SomeKey)(const scope SomeKey key) const @trusted // template-lazy, `auto ref` here makes things slow
    if (isScopedKeyType!(typeof(key)))
    {
        version(LDC) pragma(inline, true);
        assert(!key.isNull);
        import std.algorithm.searching : canFind;
        static if (isInstanceOf!(Nullable, SomeKey))
        {
            import std.traits : TemplateArgsOf;
            alias args = TemplateArgsOf!(SomeKey);
            debug static assert(args.length == 2,
                          "linear search for Nullable without nullValue is slower than default `this.contains()` and is not allowed");
            alias UnderlyingType = args[0];
            return (cast(UnderlyingType[])_bins).canFind!keyEqualPred(key.get());
        }
        else
        {
            // TODO optimize using sentinel being `key` after end of `_bins`
            foreach (const ref bin; _bins)
            {
                import std.functional : binaryFun;
                alias pred = binaryFun!keyEqualPred;
                if (pred(key, bin))
                {
                    return true;
                }
            }
            return false;
            // return _bins.canFind!keyEqualPred(key);
        }
    }

    /** Check if `element` is stored. Move found element to a hole if possible.
        Returns: `true` if element is present, `false` otherwise.
    */
    bool containsWithHoleMoving()(const scope K key) // template-lazy, `auto ref` here makes things slow
    {
        version(LDC) pragma(inline, true);
        assert(!key.isNull);
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(key);
        // TODO update holes
        return (hitIndex != _bins.length &&
                isOccupiedAtIndex(hitIndex));
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key
     * (set-case).
     *
     * If `element` is a nullable type and it is null an `AssertError` is thrown.
     */
    InsertionStatus insert()(T element) // template-lazy
    {
        version(LDC) pragma(inline, true);
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        assert(!keyOf(element).isNull);
        reserveExtra(1);
        size_t hitIndex;
        static if (isCopyable!T)
        {
            return insertWithoutGrowth(element, hitIndex);
        }
        else
        {
            return insertWithoutGrowth(move(element), hitIndex);
        }
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
    ref T insertAndReturnElement(SomeElement)(SomeElement element) return // template-lazy
    {
        version(LDC) pragma(inline, true);
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        assert(!keyOf(element).isNull);
        reserveExtra(1);
        size_t hitIndex;
        static if (isCopyable!SomeElement)
        {
            const instationStatus = insertWithoutGrowth(element, hitIndex);
        }
        else
        {
            const instationStatus = insertWithoutGrowth(move(element), hitIndex);
        }
        return _bins[hitIndex];
    }

    /** Insert `elements`, all being either a key-value (map-case) or a just a key (set-case).
     */
    void insertN(R)(R elements) @trusted
    if (isIterable!R &&
        isCopyable!T)           // TODO support uncopyable T?
    {
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        import std.range : hasLength;
        static if (hasLength!R)
        {
            reserveExtra(elements.length);
        }
        foreach (element; elements)
        {
            size_t hitIndex;
            static if (hasIndirections!T)
            {
                const instationStatus = insertWithoutGrowth(element, hitIndex);
            }
            else
            {
                const instationStatus = insertWithoutGrowth(*cast(Unqual!T*)&element, hitIndex);
            }
        }
    }

    /// Is `true` iff in-place rehashing during growth should be performed.
    enum bool growInPlaceFlag = false; // TODO warning growInPlaceWithCapacity is buggy

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
        if (newCapacity > _bins.length)
        {
            growWithNewCapacity(newCapacity);
        }
    }

    /// Grow (rehash) to make for `newCapacity` number of elements.
    private void growWithNewCapacity()(size_t newCapacity) // template-lazy
    {
        version(LDC) pragma(inline, true);
        version(showEntries) dln(__FUNCTION__, " newCapacity:", newCapacity);
        version(internalUnittest) assert(newCapacity > _bins.length);
        static if (__traits(hasMember, Allocator, "reallocate"))
        {
            static if (growInPlaceFlag)
            {
                growInPlaceWithCapacity(newCapacity);
            }
            else
            {
                growStandardWithNewCapacity(newCapacity);
            }
        }
        else
        {
            growStandardWithNewCapacity(newCapacity);
        }
    }

    private void tagAsLazilyDeletedElementAtIndex(size_t index)
    {
        pragma(inline, true);
        static if (hasHoleableKey)
        {
            keyOf(_bins[index]) = holeKeyConstant;
        }
        else
        {
            keyOf(_bins[index]).nullify();
            tagHoleAtIndex(index);
        }
        static if (hasElaborateDestructor!V) // if we should clear all
        {
            .destroy(valueOf(_bins[index]));
        }
    }

    private void insertElementAtIndex(SomeElement)(scope SomeElement element, size_t index) @trusted // template-lazy
    {
        version(LDC) pragma(inline, true);
        static if (isDynamicArray!SomeElement &&
                   !is(typeof(SomeElement.init[0]) == immutable))
        {
            /* key is an array of non-`immutable` elements which cannot safely
             * be stored because keys must be immutable for hashing to work
             * properly, therefore duplicate */
            keyOf(_bins[index]) = element.idup;
        }
        else
        {
            static if (isCopyable!SomeElement)
            {
                _bins[index] = element;
            }
            else
            {
                static if (isCopyable!K)
                {
                    keyOf(_bins[index]) = keyOf(element);
                }
                else
                {
                    move(keyOf(element), keyOf(_bins[index]));
                }
                static if (hasValue)
                {
                    moveEmplace(valueOf(element), valueOf(_bins[index]));
                }
            }
            }
    }

    /** Rehash elements in-place. */
    private void rehashInPlace()() @trusted // template-lazy
    {
        version(showEntries) dln(__FUNCTION__);

        import core.bitop : bts, bt;
        import array_help : makeZeroedBitArray, wordCountOfBitCount;

        size_t* dones = makeZeroedBitArray!Allocator(_bins.length);

        foreach (immutable doneIndex; 0 .. _bins.length)
        {
            if (bt(dones, doneIndex)) { continue; } // if _bins[doneIndex] continue
            if (isOccupiedAtIndex(doneIndex))
            {
                T currentElement = void;

                // TODO functionize:
                moveEmplace(_bins[doneIndex], currentElement);
                static if (isInstanceOf!(Nullable, K))
                {
                    keyOf(_bins[doneIndex]).nullify(); // `moveEmplace` doesn't init source of type Nullable
                }

                while (true)
                {
                    alias pred = (const scope index,
                                  const scope auto ref element) => (!isOccupiedAtIndex(index) || // free slot
                                                                    !bt(dones, index)); // or a not yet replaced element
                    immutable hitIndex = _bins[].triangularProbeFromIndex!(pred)(keyToIndex(keyOf(currentElement)));
                    assert(hitIndex != _bins.length, "no free slot");

                    bts(dones, hitIndex); // _bins[hitIndex] will be at it's correct position

                    if (isOccupiedAtIndex(doneIndex))
                    {
                        T nextElement = void;

                        // TODO functionize:
                        moveEmplace(_bins[hitIndex], nextElement); // save non-free slot
                        static if (isInstanceOf!(Nullable, K))
                        {
                            keyOf(_bins[hitIndex]).nullify(); // `moveEmplace` doesn't init source of type Nullable
                        }

                        moveEmplace(currentElement, _bins[hitIndex]);
                        moveEmplace(nextElement, currentElement);
                    }
                    else // if no free slot
                    {
                        moveEmplace(currentElement, _bins[hitIndex]);
                        break; // inner iteration is finished
                    }
                }
            }
            bts(dones, doneIndex); // _bins[doneIndex] is at it's correct position
        }

        Allocator.deallocate(cast(void[])(dones[0 .. wordCountOfBitCount(_bins.length)]));

        static if (!hasHoleableKey)
        {
            clearHoles();
        }
    }

    /** Grow (including rehash) store in-place to make room for
     * `minimumCapacity` number of elements.
     */
    private void growInPlaceWithCapacity()(size_t minimumCapacity) @trusted // template-lazy
    {
        assert(minimumCapacity > _bins.length);

        immutable powerOf2newCapacity = nextPow2(minimumCapacity);
        immutable newByteCount = T.sizeof*powerOf2newCapacity;

        const oldBinsPtr = _bins.ptr;
        immutable oldLength = _bins.length;

        auto rawBins = cast(void[])_bins;
        if (Allocator.reallocate(rawBins, newByteCount))
        {
            _bins = cast(T[])rawBins;
            static if (mustAddGCRange!T)
            {
                if (oldBinsPtr !is null)
                {
                    gc_removeRange(oldBinsPtr); // `gc_removeRange` fails for null input
                }
                gc_addRange(_bins.ptr, newByteCount);
            }

            static if (!hasHoleableKey)
            {
                if (_holesPtr)
                {
                    _holesPtr = makeReallocatedBitArrayZeroPadded!Allocator(_holesPtr,
                                                                            oldLength,
                                                                            _bins.length);
                }
            }

            // TODO make this an array operation `nullifyAll` or `nullifyN`
            foreach (ref bin; _bins[oldLength .. powerOf2newCapacity])
            {
                keyOf(bin).nullify(); // move this `init` to reallocate() above?
            }

            rehashInPlace();
        }
        else
        {
            assert(0, "couldn't reallocate bin");
        }
    }

    /** Grow (rehash) store to make room for `newCapacity` number of elements.
     */
    private void growStandardWithNewCapacity()(size_t newCapacity) // template-lazy
    {
        version(LDC) pragma(inline, true); // LDC needs this or to prevent 10x performance regression in contains()
        version(showEntries) dln(__FUNCTION__, " newCapacity:", newCapacity);
        version(internalUnittest) assert(newCapacity > _bins.length);
        auto next = typeof(this).withCapacity(newCapacity);
        foreach (immutable index, ref bin; _bins)
        {
            if (isOccupiedAtIndex(index))
            {
                next.insertMoveWithoutGrowth(bin); // value is zeroed but
                static if (!hasHoleableKey)
                {
                    keyOf(bin).nullify(); // keyC must zeroed
                }
            }
        }
        move(next, this);
    }

    private InsertionStatus insertWithoutGrowth(SomeElement)(SomeElement element, // template-lazy
                                                             out size_t hitIndex)
    {
        version(LDC) pragma(inline, true);
        version(internalUnittest)
        {
            assert(!keyOf(element).isNull);
            static if (hasHoleableKey)
            {
                assert(!isHoleKeyConstant(keyOf(element)));
            }
        }

        size_t holeIndex = size_t.max; // first hole index to written to if hole found
        immutable hitIndexPrel = indexOfKeyOrVacancyAndFirstHole(keyOf(element), holeIndex);
        if (hitIndexPrel == _bins.length || // keys miss and holes may have filled all empty slots
            keyOf(_bins[hitIndexPrel]).isNull) // just key miss but a hole may have been found on the way
        {
            immutable hasHole = holeIndex != size_t.max; // hole was found along the way
            if (hasHole)
            {
                hitIndex = holeIndex; // pick hole instead
            }
            else
            {
                hitIndex = hitIndexPrel; // normal hit
            }
            version(internalUnittest) assert(hitIndex != _bins.length, "no null or hole slot");
            static if (isCopyable!SomeElement)
            {
                insertElementAtIndex(element, hitIndex);
            }
            else
            {
                insertElementAtIndex(move(element), hitIndex);
            }
            static if (!hasHoleableKey)
            {
                if (hasHole) { untagHoleAtIndex(hitIndex); }
            }
            _count = _count + 1;
            return InsertionStatus.added;
        }
        else
        {
            hitIndex = hitIndexPrel;
        }

        static if (hasValue)
        {
            static if (isStaticArray!V)
            {
                // identity comparison of static arrays implicitly coerces them
                // to slices, which are compared by reference, so don't use !is here
                immutable valueDiffers = (valueOf(element) !=
                                          valueOf(_bins[hitIndexPrel])); // only value changed
            }
            else
            {
                immutable valueDiffers = (valueOf(element) !is
                                          valueOf(_bins[hitIndexPrel])); // only value changed
            }
            if (valueDiffers) // only value changed
            {
                move(valueOf(element),
                     valueOf(_bins[hitIndexPrel])); // value is defined so overwrite it
                return InsertionStatus.modified;
            }
        }
        return InsertionStatus.unmodified;
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    private InsertionStatus insertMoveWithoutGrowth()(ref T element) // template-lazy
    {
        version(LDC) pragma(inline, true);
        size_t hitIndex;
        return insertWithoutGrowth(move(element), hitIndex);
    }

    static if (hasValue)
    {
        /** Insert or replace `value` at `key`. */
        InsertionStatus insert()(K key, V value) // template-lazy
        {
            pragma(inline, true); // LDC must have this
            static if (isCopyable!K)
            {
                static if (isCopyable!V)
                {
                    return insert(T(key, value));
                }
                else
                {
                    return insert(T(key, move(value)));
                }
            }
            else
            {
                static if (isCopyable!V)
                {
                    return insert(T(move(key), value));
                }
                else
                {
                    return insert(T(move(key), move(value)));
                }
            }
        }
    }

    static if (!hasValue)       // HashSet
    {
        scope const(K)* opBinaryRight(string op, SomeKey)(const scope SomeKey key) const return @trusted
        if (op == `in` &&
            isScopedKeyType!(typeof(key)))
        {
            pragma(inline, true);
            assert(!key.isNull);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(K)key); // cast scoped `key` is @trusted
            return (hitIndex != _bins.length &&
                    isOccupiedAtIndex(hitIndex)) ? &_bins[hitIndex] :
            null; /* TODO instead of null return a reference to a struct SlotRef
                   * when assigned to sets value in slot and increases
                   * table._count += 1; */
        }
    }

    static if (hasValue)        // HashMap
    {
        scope inout(V)* opBinaryRight(string op, SomeKey)(const scope SomeKey key) inout return @trusted // `auto ref` here makes things slow
        if (op == `in` &&
            isScopedKeyType!(SomeKey))
        {
            version(LDC) pragma(inline, true);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(K)adjustKeyType(key)); // cast scoped `key` is @trusted
            if (hitIndex != _bins.length &&
                isOccupiedAtIndex(hitIndex))
            {
                return cast(typeof(return))&_bins[hitIndex].value;
            }
            else
            {
                return null;    // TODO return reference to where element should be placed
            }
        }

        /// Indexing.
        scope ref inout(V) opIndex(SomeKey)(const scope SomeKey key) inout return @trusted // `auto ref` here makes things slow
        if (isScopedKeyType!(typeof(key)))
        {
            version(LDC) pragma(inline, true);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(K)adjustKeyType(key)); // cast scoped `key` is @trusted
            if (hitIndex != _bins.length &&
                isOccupiedAtIndex(hitIndex))
            {
                return _bins[hitIndex].value;
            }
            version(assert)
            {
                import core.exception : RangeError;
                throw new RangeError("Key not found");
            }
        }

        /** Get value of `key` or `defaultValue` if `key` not present (and
         * therefore `nothrow`).
         *
         * Returns: value reference iff `defaultValue` is an l-value.
         *
         * TODO make `defaultValue` `lazy` when that can be `nothrow`
         */
        auto ref inout(V) get()(const scope K key, // template-lazy
                                auto ref inout(V) defaultValue) inout
        {
            auto valuePtr = key in this;
            if (valuePtr !is null)
            {
                return *valuePtr;
            }
            else
            {
                return defaultValue;
            }
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
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(cast(K)adjustKeyType(key)); // cast scoped `key` is @trusted
            if (hitIndex != _bins.length &&
                isOccupiedAtIndex(hitIndex))
            {
                return _bins[hitIndex].key;
            }
            else
            {
                return defaultKey;
            }
        }

        static if (isAddress!V)
        {
            /** Supports $(B aa[key] = value;) syntax.
             */
            V opIndexAssign()(V value, K key) // template-lazy
            {
                version(LDC) pragma(inline, true);
                static if (isCopyable!K)
                {
                    insert(T(key, value));
                }
                else
                {
                    insert(T(move(key), value));
                }
                return value;
            }
        }
        else
        {
            /** Supports $(B aa[key] = value;) syntax.
             */
            void opIndexAssign()(V value, K key) // template-lazy
            {
                version(LDC) pragma(inline, true);
                static if (isCopyable!K)
                {
                    static if (isCopyable!V)
                    {
                        insert(T(key, value));
                    }
                    else
                    {
                        insert(T(key, move(value)));
                    }
                }
                else
                {
                    static if (isCopyable!V)
                    {
                        insert(T(move(key), value));
                    }
                    else
                    {
                        insert(T(move(key), move(value)));
                    }
                }
                // TODO return scoped reference to value
            }
        }
    }

    /** Remove `element`.
     * Returns: `true` if element was removed, `false` otherwise.
     */
    bool remove(SomeKey)(const scope SomeKey key) // template-lazy
    if (isScopedKeyType!(typeof(key)))
    {
        version(LDC) pragma(inline, true);
        static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(adjustKeyType(key));
        if (hitIndex != _bins.length &&
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
        import traits_ex : isRefIterable;
        import std.range : front;

        size_t rehashingRemoveN(Keys)(const scope Keys keys) // template-lazy
        if (isRefIterable!Keys &&
            is(typeof(Keys.front == K.init)))
        {
            static if (borrowChecked) { debug assert(!isBorrowed, borrowedErrorMessage); }
            rehash!("!a.isNull && keys.canFind(a)")(); // TODO make this work
            return 0;
        }
    }

    /// Check if empty.
    @property bool empty() const { return _count == 0; }

    /// Get length (read-only).
    @property size_t length() const { return _count; }

    /// Get bin count.
    @property size_t binCount() const { return _bins.length; }

    /** Returns: get total probe count for all elements stored. */
    size_t totalProbeCount()() const // template-lazy
    {
        version(LDC) pragma(inline, true); // LDC needs this or to prevent 10x performance regression in contains()
        static if (hasValue)
        {
            auto range = byKeyValue(this);
        }
        else
        {
            auto range = byElement(this);
        }
        typeof(return) totalCount = 0;
        foreach (const ref currentElement; range)
        {
            static if (isCopyable!T)
            {
                /* don't use `auto ref` for copyable `T`'s to prevent
                 * massive performance drop for small elements when compiled
                 * with LDC. TODO remove when LDC is fixed. */
                alias pred = (const scope element) => (keyEqualPredFn(keyOf(element),
                                                                      keyOf(currentElement)));
            }
            else
            {
                alias pred = (const scope auto ref element) => (keyEqualPredFn(keyOf(element),
                                                                               keyOf(currentElement)));
            }
            totalCount += triangularProbeCountFromIndex!pred(_bins[], keyToIndex(keyOf(currentElement)));
        }
        return totalCount;
    }

    /** Returns: average probe count for all elements stored. */
    double averageProbeCount()() const // template-lazy
    {
        return cast(typeof(return))totalProbeCount/length;
    }

    /** Unsafe access to raw bins.
     *
     * Needed by wrapper containers such as SSOOpenHashSet.
     */
    inout(T)[] rawBins() inout @system pure nothrow @nogc
    {
        pragma(inline, true);
        return _bins;
    }

    static if (hasHoleableKey)
    {
        static bool isOccupiedBin(const ref T bin)
        {
            pragma(inline, true);
            if (keyOf(bin).isNull) { return false; }
            return !isHoleKeyConstant(keyOf(bin));
        }
    }

private:
    static if (hasFunctionAttributes!(Allocator.allocate, "@nogc"))
    {
        import gc_traits : NoGc;
        @NoGc T[] _bins;        // one element per bin
    }
    else
    {
        T[] _bins;              // one element per bin
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
            size_t _count;        // total number of non-null elements stored in `_bins`
        }
    }
    else
    {
        size_t _count;        // total number of non-null elements stored in `_bins`
    }

    static if (!hasHoleableKey)
    {
        static if (hasFunctionAttributes!(Allocator.allocate, "@nogc"))
        {
            @NoGc size_t* _holesPtr; // bit array describing which bin elements that has been removed (holes)
        }
        else
        {
            size_t* _holesPtr; // bit array describing which bin elements that has been removed (holes)
        }
    }

    /** Returns: bin index of `key`. */
    private size_t keyToIndex(SomeKey)(const scope SomeKey key) const @trusted
    {
        version(LDC) pragma(inline, true);

        version(none)           // TODO activate
        static if (is(hasher == hashOf)) // TODO how do we compare symbols?
        {
            return hashOf(key) & powerOf2Mask;
        }

        import digestion : hashOf2;
        return hashOf2!(hasher)(key) & powerOf2Mask;
    }

    /** Returns: current index mask from bin count. */
    private size_t powerOf2Mask() const
    {
        pragma(inline, true);
        immutable typeof(return) mask = _bins.length - 1;
        version(internalUnittest) assert((~mask ^ mask) == typeof(mask).max); // isPowerOf2(_bins.length)
        return mask;
    }

    /** Find index to `key` if it exists or to first empty slot found, skipping
     * (ignoring) lazily deleted slots.
     */
    private size_t indexOfKeyOrVacancySkippingHoles(const scope K key) const // `auto ref` here makes things slow
        @trusted
    {
        version(LDC) pragma(inline, true);
        version(internalUnittest)
        {
            assert(!key.isNull);
            static if (hasHoleableKey)
            {
                assert(!isHoleKeyConstant(key));
            }
        }
        static if (isCopyable!T)
        {
            /* don't use `auto ref` for copyable `T`'s to prevent
             * massive performance drop for small elements when compiled
             * with LDC. TODO remove when LDC is fixed. */
            static if (hasHoleableKey)
            {
                alias pred = (const scope element) => (keyOf(element).isNull ||
                                                       keyEqualPredFn(keyOf(element), key));
            }
            else
            {
                alias pred = (const scope index,
                              const scope element) => (!hasHoleAtPtrIndex(_holesPtr, index) &&
                                                       (keyOf(element).isNull ||
                                                        keyEqualPredFn(keyOf(element), key)));
            }
        }
        else
        {
            static if (hasHoleableKey)
            {
                alias pred = (const scope auto ref element) => (keyOf(element).isNull ||
                                                                keyEqualPredFn(keyOf(element), key));
            }
            else
            {
                alias pred = (const scope index,
                              const scope auto ref element) => (!hasHoleAtPtrIndex(_holesPtr, index) &&
                                                                (keyOf(element).isNull ||
                                                                 keyEqualPredFn(keyOf(element), key)));
            }
        }
        return _bins[].triangularProbeFromIndex!(pred)(keyToIndex(key));
    }

    private size_t indexOfKeyOrVacancyAndFirstHole(SomeKey)(const scope SomeKey key, // `auto ref` here makes things slow
                                                            ref size_t holeIndex) const
        @trusted
    {
        version(LDC) pragma(inline, true);
        version(internalUnittest)
        {
            assert(!key.isNull);
            static if (hasHoleableKey)
            {
                assert(!isHoleKeyConstant(key));
            }
        }
        static if (isCopyable!T)
        {
            /* don't use `auto ref` for copyable `T`'s to prevent
             * massive performance drop for small elements when compiled
             * with LDC. TODO remove when LDC is fixed. */
            static if (hasHoleableKey)
            {
                alias hitPred = (const scope element) => (keyOf(element).isNull ||
                                                          keyEqualPredFn(keyOf(element), key));
                alias holePred = (const scope element) => (isHoleKeyConstant(keyOf(element)));
            }
            else
            {
                alias hitPred = (const scope index,
                                 const scope element) => (!hasHoleAtPtrIndex(_holesPtr, index) &&
                                                          (keyOf(element).isNull ||
                                                           keyEqualPredFn(keyOf(element), key)));
                alias holePred = (const scope index, // TODO use only index
                                  const scope element) => (hasHoleAtPtrIndex(_holesPtr, index));
            }
        }
        else
        {
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
                alias holePred = (const scope index, // TODO use only index
                                  const scope auto ref element) => (hasHoleAtPtrIndex(_holesPtr, index));
            }
        }
        return _bins[].triangularProbeFromIndexIncludingHoles!(hitPred, holePred)(keyToIndex(key),
                                                                                  holeIndex);
    }

    /** Returns: `true` iff `index` indexes a non-null element, `false`
     * otherwise.
     */
    private bool isOccupiedAtIndex(size_t index) const
    {
        pragma(inline, true);
        version(internalUnittest) assert(index < _bins.length);
        if (keyOf(_bins[index]).isNull) { return false; }
        static if (hasHoleableKey)
        {
            return !isHoleKeyConstant(keyOf(_bins[index]));
        }
        else
        {
            return !hasHoleAtPtrIndex(_holesPtr, index);
        }
    }
}

/** Duplicate `src` into uninitialized `dst` ignoring prior destruction of `dst`.
 * TODO move to more generic place
 */
static private void duplicateEmplace(T)(const scope ref T src,
                                        scope ref T dst) @system
{
    pragma(inline, true);
    import std.traits : hasElaborateCopyConstructor, isCopyable, isBasicType, isInstanceOf;
    static if (!hasElaborateCopyConstructor!T)
    {
        import std.typecons : Nullable;
        static if (is(T == class) ||
                   is(T == string))
        {
            dst = cast(T)src;
        }
        else static if (isBasicType!T ||
                        isInstanceOf!(Nullable, T)) // `Nullable` types cannot be emplaced
        {
            dst = src;
        }
        else                    // TODO can this case occur?
        {
            import std.conv : emplace;
            import std.traits : Unqual;
            emplace(&dst, cast(Unqual!T)src);
        }
    }
    else static if (__traits(hasMember, T, "dup"))
    {
        import std.conv : emplace;
        // TODO when `emplace` can handle src being an r-value of uncopyable types replace with: `emplace(&dst, src.dup);`
        emplace(&dst);
        dst = src.dup;
    }
    else
    {
        debug static assert(0, "cannot duplicate a " ~ T.stringof);
    }
}

/** L-value element reference (and in turn range iterator).
 */
static private struct LvalueElementRef(Table)
{
    import std.traits : isMutable;
    debug static assert(isMutable!Table, "Table type should always be mutable");

    private Table* _table;      // scoped access
    private size_t _binIndex;   // index to bin inside `table`
    private size_t _hitCounter; // counter over number of elements popped (needed for length)

    this(Table* table) @trusted
    {
        pragma(inline, true);
        this._table = table;
        static if (Table.isBorrowChecked)
        {
            debug
            {
                _table.incBorrowCount();
            }
        }
    }

    ~this() @trusted
    {
        pragma(inline, true);
        static if (Table.isBorrowChecked)
        {
            debug
            {
                _table.decBorrowCount();
            }
        }
    }

    this(this) @trusted
    {
        pragma(inline, true);
        static if (Table.isBorrowChecked)
        {
            debug
            {
                assert(_table._borrowCount != 0);
                _table.incBorrowCount();
            }
        }
    }

    /// Check if empty.
    @property bool empty() const @safe pure nothrow @nogc
    {
        pragma(inline, true);
        return _binIndex == _table.binCount;
    }

    /// Get number of element left to pop.
    @property size_t length() const @safe pure nothrow @nogc
    {
        pragma(inline, true);
        return _table.length - _hitCounter;
    }

    @property typeof(this) save() // ForwardRange
    {
        pragma(inline, true);
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
        pragma(inline, true);
        while (_binIndex != (*_table).binCount &&
               !(*_table).isOccupiedAtIndex(_binIndex))
        {
            _binIndex += 1;
        }
    }
}

/** R-value element reference (and in turn range iterator).
 *
 * Does need to do borrow-checking.
 */
static private struct RvalueElementRef(Table)
{
    import std.traits : isMutable;
    debug static assert(isMutable!Table, "Table type should always be mutable");

    Table _table;                // owned table
    size_t _binIndex;            // index to bin inside table
    size_t _hitCounter;    // counter over number of elements popped

    /// Check if empty.
    @property bool empty() const @safe pure nothrow @nogc
    {
        pragma(inline, true);
        return _binIndex == _table.binCount;
    }

    /// Get number of element left to pop.
    @property size_t length() const @safe pure nothrow @nogc
    {
        pragma(inline, true);
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
        pragma(inline, true);
        while (_binIndex != _table.binCount &&
               !_table.isOccupiedAtIndex(_binIndex))
        {
            _binIndex += 1;
        }
    }
}

/** Immutable hash set storing keys of type `K`.
 */
alias OpenHashSet(K,
                  alias hasher = hashOf,
                  string keyEqualPred = defaultKeyEqualPredOf!K,
                  alias Allocator = Mallocator.instance,
                  bool borrowChecked = false) = OpenHashMapOrSet!(K, void, hasher, keyEqualPred, Allocator, borrowChecked);

/** Immutable hash map storing keys of type `K` and values of type `V`.
 */
alias OpenHashMap(K, V,
                  alias hasher = hashOf,
                  string keyEqualPred = defaultKeyEqualPredOf!K,
                  alias Allocator = Mallocator.instance,
                  bool borrowChecked = false) = OpenHashMapOrSet!(K, V, hasher, keyEqualPred, Allocator, borrowChecked);

import std.traits : isInstanceOf;
import std.functional : unaryFun;

/** Remove all elements in `x` matching `pred`.
 *
 * TODO make this generic for all iterable containers and move to
 * container_algorithm.
 */
size_t removeAllMatching(alias pred, Table)(auto ref Table x) @trusted
    if (isInstanceOf!(OpenHashMapOrSet, Table) && // TODO generalize to `isSetOrMap`
        is(typeof((unaryFun!pred))))
{
    import container_traits : nullify;
    size_t removalCount = 0;
    foreach (immutable index, ref bin; x._bins)
    {
        // TODO:
        // move to Table.removeRef(bin) // uses: `offset = &bin - _bins.ptr`
        // move to Table.inplaceRemove(bin) // uses: `offset = &bin - _bins.ptr`
        // or   to Table.removeAtIndex(index)
        if (x.isOccupiedAtIndex(index) &&
            unaryFun!pred(bin))
        {
            x.tagAsLazilyDeletedElementAtIndex(index);
            removalCount += 1;
        }
    }
    x._count = x._count - removalCount;
    return removalCount;        // TODO remove this return value
}

/** Returns: `x` eagerly filtered on `pred`.
    TODO move to container_algorithm.d with more generic template restrictions
*/
Table filtered(alias pred, Table)(Table x)
    if (isInstanceOf!(OpenHashMapOrSet, Table)) // TODO generalize to `isSetOrMap`
{
    import std.functional : not;
    x.removeAllMatching!(not!pred); // `x` is a singleton (r-value) so safe to mutate
    import std.algorithm.mutation : move;
    return move(x);             // functional
}

/** Returns: `x` eagerly intersected with `y`.
    TODO move to container_algorithm.d.
 */
auto intersectedWith(C1, C2)(C1 x, auto ref C2 y)
    if (isInstanceOf!(OpenHashMapOrSet, C1) && // TODO generalize to `isSetOrMap`
        isInstanceOf!(OpenHashMapOrSet, C2))   // TODO generalize to `isSetOrMap`
{
    import std.algorithm.mutation : move;
    static if (__traits(isRef, y)) // y is l-value
    {
        // @("complexity", "O(x.length)")
        return move(x).filtered!(_ => y.contains(_)); // only x can be reused
    }
    else
    {
        /* both are r-values so reuse the shortest */
        // @("complexity", "O(min(x.length), min(y.length))")
        if (x.length <
            y.length)
        {
            return move(x).filtered!(_ => y.contains(_)); // functional
        }
        else
        {
            return move(y).filtered!(_ => x.contains(_)); // functional
        }
    }
}

/// exercise opEquals
@safe pure nothrow @nogc
unittest
{
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
    () @trusted { assert(a._bins != b._bins); }();
}

@safe pure nothrow /*@nogc*/ unittest
{
    enum Pot { noun, verb }
    struct ExprPot
    {
        string expr;
        Pot pot;

        alias nullifier = expr;
        static immutable nullValue = typeof(this).init;

        bool opEquals(const scope typeof(this) that) const @safe pure nothrow @nogc
        {
            return (this.expr == that.expr &&
                    this.pot == that.pot);
        }
    }

    import digestx.fnv : FNV;
    alias X = OpenHashSet!(ExprPot, FNV!(64, true));

    X x;

    const aa = "aa";
    const key1 = ExprPot(aa[0 .. 1], Pot.noun);
    const key2 = ExprPot(aa[1 .. 2], Pot.noun);

    assert(key1 == key2);
    assert(key1 !is key2);

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
    version(showEntries) dln();
    import digestx.fnv : FNV;

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

    assert("a".ptr is "a".ptr); // string literals are store in common place

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
    // TODO this should fail:
    // TODO debug static assert(!__traits(compiles, { testEscapeShouldFailFront(); } ));
}

/// `string` as key
@safe pure nothrow unittest
{
    version(showEntries) dln();
    import digestx.fnv : FNV;
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
    version(showEntries) dln();

    import basic_array : Array = BasicArray;

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
            // TODO assert(x.containsUsingLinearSearch(key));
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
    version(showEntries) dln();
    alias K = Nullable!(uint, uint.max);
    alias X = OpenHashSet!(K, FNV!(64, true));

    auto x = X();

    {                           // scoped range
        foreach (ref xe; x.byElement) { assert(0); }
    }

    auto x0 = X.init;
    assert(x0.length == 0);
    assert(x0._bins.length == 0);
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

    import std.algorithm.mutation : move;
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
    version(showEntries) dln();
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
    TODO move to container_algorithm.d.
 */
auto intersectWith(C1, C2)(ref C1 x,
                           auto ref const(C2) y)
    if (isInstanceOf!(OpenHashMapOrSet, C1) &&
        isInstanceOf!(OpenHashMapOrSet, C2))
{
    return x.removeAllMatching!(_ => !y.contains(_));
}

/// r-value and l-value intersection
@safe pure nothrow @nogc unittest
{
    version(showEntries) dln();
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
static struct ByLvalueElement(Table) // public for now because this is needed in `knet.zing.Zing.EdgesOfRels`
{
    @disable this(this);
pragma(inline, true):
    // TODO functionize
    import std.traits : isMutable;
    static if (isAddress!(Table.ElementType)) // for reference types
    {
        /// Get reference to front element.
        @property scope Table.ElementType front()() return @trusted
        {
            // cast to head-const for class key
            return (cast(typeof(return))_table._bins[_binIndex]);
        }
    }
    else
    {
        /// Get reference to front element.
        @property scope auto ref front() return @trusted
        {
            return *(cast(const(Table.ElementType)*)&_table._bins[_binIndex]); // propagate constnes
        }
    }
    import std.traits : Unqual;
    // unqual to reduce number of instantations of `LvalueElementRef`
    public LvalueElementRef!(Unqual!Table) _elementRef;
    alias _elementRef this;
}

/// Range over elements of r-value instance of this.
static private struct ByRvalueElement(Table)
{
    @disable this(this);
pragma(inline, true):
    static if (isAddress!(Table.ElementType)) // for reference types
    {
        /// Get reference to front element.
        @property scope Table.ElementType front()() return @trusted
        {
            // cast to head-const for class key
            return cast(typeof(return))_table._bins[_binIndex];
        }
    }
    else
    {
        /// Get reference to front element.
        @property scope auto ref front() return
        {
            return *(cast(const(Table.ElementType)*)&_table._bins[_binIndex]); // propagate constnes
        }
    }
    import std.traits : Unqual;
    public RvalueElementRef!(Unqual!Table) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the elements of `c` in undefined order.
 */
auto byElement(Table)(auto ref return Table c) @trusted
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    !Table.hasValue)
{
    import std.traits : Unqual;
    alias M = Unqual!Table;
    alias C = const(Table);        // be const for now
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByLvalueElement!C((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import std.algorithm.mutation : move;
        auto result = ByRvalueElement!C((RvalueElementRef!(M)(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}
alias range = byElement;        // EMSI-container naming

static private struct ByKey_lvalue(Table)
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    @property scope const auto ref front() return // key access must be const, TODO auto ref => ref K
    {
        pragma(inline, true);
        return _table._bins[_binIndex].key;
    }
    import std.traits : Unqual;
    public LvalueElementRef!(Unqual!Table) _elementRef;
    alias _elementRef this;
}

static private struct ByKey_rvalue(Table)
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    @property scope const auto ref front() return // key access must be const, TODO auto ref => ref K
    {
        pragma(inline, true);
        return _table._bins[_binIndex].key;
    }
    import std.traits : Unqual;
    public RvalueElementRef!(Unqual!Table) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the keys of `c` in undefined order.
 */
auto byKey(Table)(auto ref /*TODO return*/ Table c) @trusted
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    import std.traits : Unqual;
    alias M = Unqual!Table;
    alias C = const(Table);        // be const
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByKey_lvalue!C((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import std.algorithm.mutation : move;
        auto result = ByKey_rvalue!C((RvalueElementRef!M(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

static private struct ByValue_lvalue(Table)
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    @property scope auto ref front() return @trusted // TODO auto ref => ref V
    {
        pragma(inline, true);
        // TODO functionize
        import std.traits : isMutable;
        static if (isMutable!(Table)) // TODO can this be solved without this `static if`?
        {
            alias E = Table.ValueType;
        }
        else
        {
            alias E = const(Table.ValueType);
        }
        return *(cast(E*)&_table._bins[_binIndex].value);
    }
    import std.traits : Unqual;
    public LvalueElementRef!(Unqual!Table) _elementRef;
    alias _elementRef this;
}

static private struct ByValue_rvalue(Table)
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    @property scope auto ref front() return @trusted // TODO auto ref => ref V
    {
        pragma(inline, true);
        // TODO functionize
        import std.traits : isMutable;
        static if (isMutable!(Table)) // TODO can this be solved without this `static if`?
        {
            alias E = Table.ValueType;
        }
        else
        {
            alias E = const(Table.ValueType);
        }
        return *(cast(E*)&_table._bins[_binIndex].value);
    }
    import std.traits : Unqual;
    public RvalueElementRef!(Unqual!Table) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the values of `c` in undefined order.
 */
auto byValue(Table)(auto ref return Table c) @trusted
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    import std.traits : Unqual;
    import std.traits : isMutable;
    alias M = Unqual!Table;
    alias C = const(Table);
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByValue_lvalue!Table((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import std.algorithm.mutation : move;
        auto result = ByValue_rvalue!C((RvalueElementRef!M(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

static private struct ByKeyValue_lvalue(Table)
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    @property scope auto ref front() return @trusted // TODO auto ref => ref T
    {
        pragma(inline, true);
        // TODO functionize
        import std.traits : isMutable;
        static if (isMutable!(Table))
        {
            alias E = Table.KeyValueType;
        }
        else
        {
            alias E = const(Table.T);
        }
        return *(cast(E*)&_table._bins[_binIndex]);
    }
    import std.traits : Unqual;
    public LvalueElementRef!(Unqual!Table) _elementRef;
    alias _elementRef this;
}

/** Returns: range that iterates through the key-value-pairs of `c` in undefined order.
 */
auto byKeyValue(Table)(auto ref return Table c) @trusted
if (isInstanceOf!(OpenHashMapOrSet, Table) &&
    Table.hasValue)
{
    import std.traits : Unqual;
    alias M = Unqual!Table;
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = ByKeyValue_lvalue!Table((LvalueElementRef!(M)(cast(M*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import std.algorithm.mutation : move;
        auto result = ByKeyValue_rvalue!Table((RvalueElementRef!M(move(*(cast(M*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

/// make range from l-value and r-value. element access is always const
@system pure unittest
{
    version(showEntries) dln();
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
    // TODO assert(x.byElement.count == x.length);
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
@trusted pure unittest
{
    version(showEntries) dln();
    immutable n = 11;

    alias K = Nullable!(uint, uint.max);
    alias V = uint;

    alias X = OpenHashMapOrSet!(K, V, FNV!(64, true));

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
@trusted pure unittest
{
    version(showEntries) dln();
    immutable n = 11;

    alias K = Nullable!(uint, uint.max);
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = OpenHashMapOrSet!(K, V, FNV!(64, true));

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
    version(showEntries) dln();
    alias K = Nullable!(uint, uint.max);
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = OpenHashMapOrSet!(K, V, FNV!(64, true));
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
    version(showEntries) dln();
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

    alias X = OpenHashMapOrSet!(K, V, FNV!(64, true));
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
    version(showEntries) dln();
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

    alias X = OpenHashMapOrSet!(K, V, FNV!(64, true));
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
    version(showEntries) dln();
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

    alias X = OpenHashMapOrSet!(K, V, FNV!(64, true));
    auto x = X();

    auto key42 = new K(42);
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
    version(showEntries) dln();
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

        assert(x._bins.ptr !is y._bins.ptr);
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

        @property void toString(scope void delegate(const(char)[]) sink) const
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
        foreach (V; AliasSeq!(string,
                              int,
                              void))
        {
            alias X = OpenHashMapOrSet!(K, V, FNV!(64, true));

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
                import std.range : isInputRange;
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

                // TODO assert(x.byElement.count == 3);

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

                // TODO assert(y.byElement.count == 3);
                assert(x == y);

                const z = X();
                // TODO assert(z.byElement.count == 0);

                immutable w = X();
                // TODO assert(w.byElement.count == 0);

                {
                    auto xc = X.withElements([k11, k12, k13].s);
                    assert(xc.length == 3);
                    assert(xc.contains(k11));
                    assert(xc.containsUsingLinearSearch(k11));

                    // TODO http://forum.dlang.org/post/kvwrktmameivubnaifdx@forum.dlang.org
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
                import basic_array : Array = BasicArray;
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
    version(showEntries) dln();
    alias X = OpenHashMapOrSet!(Nullable!(size_t, size_t.max), size_t, FNV!(64, true));
    import basic_array : Array = BasicArray;
    X x;
    // TODO these segfault:
    // TODO auto a = Array!(X.KeyType).withElementsOfRange_untested(x.byKey); // l-value byKey
    // TODO auto b = Array!(X.KeyType).withElementsOfRange_untested(X().byKey); // r-value byKey
}

/// manual Nullable type
@safe pure unittest
{
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

    struct ZingRel
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
    debug static assert(isNullable!ZingRel);

    alias X = OpenHashSet!(ZingRel, FNV!(64, true));
    debug static assert(X.sizeof == 32); // TODO fix hole handling and change to 24
    X x;

    auto e = ZingRel(new Zing(42), Alt.init);

    assert(!x.contains(e));
    assert(!x.containsUsingLinearSearch(e));
    assert(x.insert(e) == X.InsertionStatus.added);
    assert(x.contains(e));
    assert(x.containsUsingLinearSearch(e));
}

/// class type with default hashing
@safe unittest
{
    static class Base
    {
        @safe pure nothrow @nogc:
        this(ulong value) { this._value = value; }
        @property bool opEquals(const scope typeof(this) rhs) const
        {
            return _value == rhs._value;
        }
        private ulong _value;
    }

    static class Node : Base
    {
        @safe pure nothrow @nogc:
        this(ulong value) { super(value);  }
    }
    debug static assert(is(Node : Base));

    alias X = OpenHashSet!(Base, hashOf, "a && b && (typeid(a) is typeid(b)) && a.opEquals(b)");
    debug static assert(X.sizeof == 24);
    X x;

    // top-class
    auto b42 = new Base(42);
    assert(!x.contains(b42));
    assert(!x.containsUsingLinearSearch(b42));
    assert(x.insert(b42) == X.InsertionStatus.added);
    assert(x.contains(b42));
    assert(x.containsUsingLinearSearch(b42));

    // top-class
    auto b43 = new Base(43);
    assert(!x.contains(b43));
    assert(!x.containsUsingLinearSearch(b43));
    assert(x.insert(b43) == X.InsertionStatus.added);
    assert(x.contains(b43));
    assert(x.containsUsingLinearSearch(b43));

    // sub-class
    auto n42 = new Node(42);
    assert(!x.contains(n42));     // mustn't equal to `b42`
    assert(!x.containsUsingLinearSearch(n42)); // mustn't equal to `b42`
    assert(x.insert(n42) == X.InsertionStatus.added); // added as separate type
    assert(x.contains(n42));
    assert(x.containsUsingLinearSearch(n42));

    assert(hashOf(b42) != hashOf(n42));

    // sub-class
    auto n43 = new Node(43);
    assert(!x.contains(n43));     // mustn't equal to `b43`
    assert(!x.containsUsingLinearSearch(n43)); // mustn't equal to `b43`
    assert(x.insert(n43) == X.InsertionStatus.added); // added as separate type
    assert(x.contains(n43));
    assert(x.containsUsingLinearSearch(n43));

    assert(hashOf(b43) != hashOf(n43));
}

/// enumeration key
@safe pure unittest
{
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
    static struct Rel
    {
        static immutable nullValue = typeof(this).init;
        string name;            // relation name
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
        /* TODO assert(x.containsUsingLinearSearch(Rel(ch.idup))); */
    }
}

/// `SSOString` as set key type
@safe pure nothrow @nogc
unittest
{
    import sso_string : SSOString;
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
        // TODO assert(a.insertAndReturnElement(K(ch)) == k);
        assert(a.contains(k));
        assert(a.containsUsingLinearSearch(k));
    }

    X b;
    foreach (const i; 0 .. n)
    {
        const char[1] ch = ['a' + (n - 1 - i)];
        const k = K(ch);        // @nogc
        assert(!b.contains(k));
        assert(!b.containsUsingLinearSearch(k));
        assert(b.insert(K(ch)) == X.InsertionStatus.added);
        // TODO assert(b.insertAndReturnElement(K(ch)) == k);
        assert(b.contains(k));
        assert(b.containsUsingLinearSearch(k));
    }

    assert(a == b);
}

/// `SSOString` as map key type
@trusted pure nothrow @nogc
unittest
{
    import sso_string : SSOString;
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
        // TODO assert(a.getKeyRef(ch, default_k)[] is default_k[]); // on miss use `default_k`

        a[k] = V.init;

        assert(a.contains(k));
        assert(a.contains(ch[]));                    // @nogc
        assert(a.getKeyRef(k, default_k)[] !is k[]); // on hit doesn't use `default_k`
        assert(a.getKeyRef(k, default_k)[] == ch);
        // TODO assert(a.getKeyRef(ch, default_k)[] !is k[]); // on hit doesn't use `default_k`
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
        // TODO assert(a.getKeyRef(ch, default_k)[] is default_k[]); // on miss use `default_k`

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

version(unittest)
{
    debug import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError, AssertError;
    import std.algorithm : count;
    import std.algorithm.comparison : equal;
    import std.typecons : Nullable;
    import std.meta : AliasSeq;

    import container_traits : mustAddGCRange;
    import digestx.fnv : FNV;
    import array_help : s;

    import dbgio;
}
