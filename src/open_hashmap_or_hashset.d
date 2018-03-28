module open_hashmap_or_hashset;

// version = showEntries;
// version = internalUnittest; // fed by dub (see dub.sdl) in unittest-internal mode

import container_traits : isNullable;
import pure_mallocator : PureMallocator;

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
 *
 * See_Also: https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/
 * See_Also: https://en.wikipedia.org/wiki/Lazy_deletion
 * See_Also: https://forum.dlang.org/post/ejqhcsvdyyqtntkgzgae@forum.dlang.org
 *
 * TODO remove dependency on Nullable and use bitarrays for nulls and holes
 *
 * TODO alternatively use extra template arguments for nulls and hole values for
 * instance uint.max and uint.max-1
 *
 * TODO if hash-function is cast(size_t)(classInstance) always use prime length
 * and shift pointer before hash based on alignof (might not be needed when
 * module prime) to maximize memory locality when adding successively allocated
 * pointers
 *
 * TODO keep only predicates with ref arguments when LDC can optimize those as
 * fast as value passing. add LDC issue for this
 *
 * TODO fix bug in `growInPlaceWithCapacity` and benchmarka
 *
 * TODO add extractElement that moves it out similar to
 * http://en.cppreference.com/w/cpp/container/unordered_set/extract
 *
 * TODO add merge or union algorithm here or into container_algorithm.d. See
 * also: http://en.cppreference.com/w/cpp/container/unordered_set/merge. this
 * algorithm moves elements from source if they are not already in `this`
 *
 * TODO Robin-Hood-hashing
 */
struct OpenHashMapOrSet(K, V = void,
                        alias hasher = hashOf,
                        alias Allocator = PureMallocator.instance)
    if (isNullable!K
        // isHashable!K
        )
{
    import std.algorithm.mutation : move, moveEmplace;
    import std.conv : emplace;
    import std.math : nextPow2;
    import std.traits : hasElaborateDestructor, isCopyable, isMutable, hasIndirections,
        isPointer, isDynamicArray, Unqual, hasFunctionAttributes;
    import std.typecons : Nullable;

    import container_traits : defaultNullKeyConstantOf, mustAddGCRange, isNull, nullify;
    import qcmeman : gc_addRange, gc_removeRange;
    import digestion : hashOf2;
    import probing : triangularProbeFromIndex, triangularProbeCountFromIndex;

    /** In the hash map case, `V` is non-void, and a value is stored alongside
     * the key of type `K`.
     */
    enum hasValue = !is(V == void);

    /** Is `true` iff `K` is an address, in which case holes are represented by
     * a specific value `holeKeyConstant`.
     */
    enum hasAddressKey = (is(K == class) || isPointer!K || isDynamicArray!K);

    static if (hasAddressKey)
    {
        enum holeKeyOffset = 0x1;
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
        import core.bitop : bts, bt, btr;
        import array_help : makeUninitializedBitArray, makeZeroedBitArray, makeReallocatedBitArrayZeroPadded;
    }

    alias MutableThis = Unqual!(typeof(this));
    alias ConstThis = const(MutableThis);

    static if (isInstanceOf!(Nullable, K))
    {
        alias WrappedKey = Unqual!(typeof(K.get));
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
        static auto ref inout(K) keyOf()(auto ref return scope inout(T) element)
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

        enum keyEqualPred = "a.key is b";

        enum nullKeyElement = T(defaultNullKeyConstantOf!K, V.init);
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
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            pragma(inline, true);
            return element;
        }

        enum keyEqualPred = "a is b";

        enum nullKeyElement = defaultNullKeyConstantOf!K;
    }

    alias ElementType = T;

    /** Make with room for storing at least `capacity` number of elements.
     *
     * See_Also:
     * https://forum.dlang.org/post/nyngzsaeqxzzuumivtze@forum.dlang.org
     */
    static typeof(this) withCapacity()(size_t capacity) // template-lazy
    {
        version(LDC) pragma(inline, true);
        version(showEntries) dln(__FUNCTION__, " capacity:", capacity);
        return typeof(return)(makeDefaultInitializedBins(capacity), 0);
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

        immutable byteCount = T.sizeof*capacity;

        static if (hasAddressKey) // addresses are always default-initialized to zero (null)
        {
            /* prefer call to calloc before malloc+memset:
             * https://stackoverflow.com/questions/2688466/why-mallocmemset-is-slower-than-calloc */
            // TODO functionize to `makeZeroInitArray`
            static if (__traits(hasMember, Allocator, "zeroallocate"))
            {
                auto bins = cast(T[])Allocator.instance.zeroallocate(byteCount);
            }
            else
            {
                auto bins = cast(T[])Allocator.instance.allocate(byteCount);
                import core.stdc.string : memset;
                memset(bins.ptr, 0, byteCount);
            }
        }
        else                    // when default null key is not represented by zeros
        {
            // TODO detect when initial zeroing is enough and use also here
            auto bins = cast(T[])Allocator.instance.allocate(byteCount);
            foreach (ref bin; bins)
            {
                // TODO make this work
                // static if (__traits(hasMember, K , "nullValue"))
                // {
                //     emplace(&keyOf(bin), K.nullValue);
                // }
                // else
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
        auto bins = cast(typeof(return))Allocator.instance.allocate(byteCount);
        static if (mustAddGCRange!T)
        {
            gc_addRange(bins.ptr, byteCount);
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
            foreach (ref element; elements)
            {
                that.insertWithoutGrowth(element);
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
        static if (!hasAddressKey)
        {
            if (_holesPtr)
            {
                immutable wordCount = holesWordCount(_bins.length);

                auto holesPtrCopy = makeUninitializedBitArray!Allocator(_bins.length);
                holesPtrCopy[0 .. wordCount] = _holesPtr[0 .. wordCount]; // TODO use memcpy instead?

                return typeof(return)(binsCopy, _count, holesPtrCopy);
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

        static if (!hasAddressKey)
        {
            void deallocateHoles() @trusted
            {
                if (_holesPtr)
                {
                    static if (__traits(hasMember, Allocator, "deallocatePtr"))
                    {
                        Allocator.instance.deallocatePtr(_holesPtr);
                    }
                    else
                    {
                        Allocator.instance.deallocate(_holesPtr[0 .. holesWordCount(_bins.length)]);
                    }
                }
            }

            static size_t* reallocateHoles(size_t[] holes, size_t byteCount) @trusted
            {
                auto rawHoles = cast(void[])holes;
                const ok = Allocator.instance.reallocate(rawHoles, byteCount);
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
            static if (!hasAddressKey)
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

    static immutable borrowedErrorMessage = "cannot mutate this when it's borrowed";

    /// Empty.
    void clear()()              // template-lazy
    {
        debug assert(!isBorrowed, borrowedErrorMessage);
        release();
        _bins = typeof(_bins).init;
        static if (!hasAddressKey)
        {
            _holesPtr = null;
        }
        _count = 0;
    }

    /// Release internal allocations.
    private void release()
    {
        releaseBinElements();
        releaseBinsAndHolesSlices();
    }

    /// Release bin elements.
    private void releaseBinElements()
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
    private void releaseBinsAndHolesSlices()
    {
        releaseBinsSlice(_bins);
        static if (!hasAddressKey)
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
            Allocator.instance.deallocatePtr(bins.ptr);
        }
        else
        {
            Allocator.instance.deallocate(bins);
        }
    }

    /** Check if `element` is stored.
        Returns: `true` if element is present, `false` otherwise.
    */
    bool contains()(const scope K key) const // template-lazy, `auto ref` here makes things slow
    {
        version(LDC) pragma(inline, true);
        assert(!key.isNull);
        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(key);
        return (hitIndex != _bins.length &&
                isOccupiedAtIndex(hitIndex));
    }
    static if (isInstanceOf!(Nullable, K))
    {
        bool contains(const scope WrappedKey wrappedKey) const // template-lazy, `auto ref` here makes things slow
        {
            pragma(inline, true);
            return contains(K(wrappedKey));
        }
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key
     * (set-case).
     *
     * If `element` is a nullable type and it is null an `AssertError` is thrown.
     */
    InsertionStatus insert()(T element) // template-lazy
    {
        version(LDC) pragma(inline, true);
        debug assert(!isBorrowed, borrowedErrorMessage);
        assert(!keyOf(element).isNull);
        reserveExtra(1);
        return insertWithoutGrowth(move(element));
    }
    static if (!hasValue &&
               isInstanceOf!(Nullable, K))
    {
        InsertionStatus insert()(WrappedKey wrappedElement) // template-lazy
        {
            pragma(inline, true);
            return insert(K(wrappedElement));
        }
    }

    /** Insert `elements`, all being either a key-value (map-case) or a just a key (set-case).
     */
    void insertN(R)(R elements) @trusted
        if (isIterable!R &&
            isCopyable!T)       // TODO support uncopyable T?
    {
        debug assert(!isBorrowed, borrowedErrorMessage);
        import std.range : hasLength;
        static if (hasLength!R)
        {
            reserveExtra(elements.length);
        }
        foreach (element; elements)
        {
            static if (hasIndirections!T)
            {
                insertWithoutGrowth(element);
            }
            else
            {
                insertWithoutGrowth(*cast(Unqual!T*)&element);
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
        debug assert(!isBorrowed, borrowedErrorMessage);
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
        static if (hasAddressKey)
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

    private void insertMoveElementAtIndex()(ref T element, size_t index) @trusted // template-lazy
    {
        version(LDC) pragma(inline, true);
        move(keyOf(element), keyOf(_bins[index]));
        static if (hasValue)
        {
            moveEmplace(valueOf(element), valueOf(_bins[index]));
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

        Allocator.instance.deallocate(cast(void[])(dones[0 .. wordCountOfBitCount(_bins.length)]));

        static if (!hasAddressKey)
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
        if (Allocator.instance.reallocate(rawBins, newByteCount))
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

            static if (!hasAddressKey)
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
                static if (!hasAddressKey)
                {
                    keyOf(bin).nullify(); // keyC must zeroed
                }
            }
        }
        move(next, this);
    }

    private InsertionStatus insertWithoutGrowth()(T element)  // template-lazy
    {
        version(LDC) pragma(inline, true);
        version(internalUnittest)
        {
            assert(!keyOf(element).isNull);
            static if (hasAddressKey)
            {
                assert(!isHoleKeyConstant(keyOf(element)));
            }
        }

        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(keyOf(element));
        if (hitIndex == _bins.length || // keys miss and holes may have filled all empty slots
            keyOf(_bins[hitIndex]).isNull) // just key miss but a hole may be have be found on the way
        {
            immutable hitIndex1 = indexOfHoleOrNullForKey(keyOf(element)); // try again to reuse hole
            version(internalUnittest) assert(hitIndex1 != _bins.length, "no null or hole slot");
            insertMoveElementAtIndex(element, hitIndex1);
            static if (!hasAddressKey)
            {
                untagHoleAtIndex(hitIndex1);
            }
            _count = _count + 1;
            return InsertionStatus.added;
        }

        static if (hasValue)
        {
            if (valueOf(element) !is
                valueOf(_bins[hitIndex])) // only value changed
            {
                move(valueOf(element),
                     valueOf(_bins[hitIndex])); // value is defined so overwrite it
                return InsertionStatus.modified;
            }
        }
        return InsertionStatus.unmodified;
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    private InsertionStatus insertMoveWithoutGrowth()(ref T element) // template-lazy
    {
        pragma(inline, true);
        return insertWithoutGrowth(move(element));
    }

    static if (hasValue)
    {
        /** Insert or replace `value` at `key`. */
        pragma(inline, true)    // LDC must have this
        InsertionStatus insert()(K key, V value) // template-lazy
        {
            return insert(T(move(key),
                            move(value)));
        }
        static if (isInstanceOf!(Nullable, K))
        {
            pragma(inline, true)    // LDC must have this
            InsertionStatus insert()(WrappedKey wrappedKey, V value) // template-lazy
            {
                return insert(K(wrappedKey), move(value));
            }
        }
    }

    static if (!hasValue)       // HashSet
    {
        scope const(K)* opBinaryRight(string op)(const scope K key) const return
            if (op == "in")
        {
            pragma(inline, true);
            assert(!key.isNull);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(key);
            return (hitIndex != _bins.length &&
                    isOccupiedAtIndex(hitIndex)) ? &_bins[hitIndex] :
            null; /* TODO instead of null return a reference to a struct SlotRef
                   * when assigned to sets value in slot and increases
                   * table._count += 1; */
        }
        static if (isInstanceOf!(Nullable, K))
        {
            scope const(K)* opBinaryRight(string op)(const scope WrappedKey wrappedKey) const return
                if (op == "in")
            {
                pragma(inline, true);
                return opBinaryRight!"in"(K(wrappedKey));
            }
        }

        /// Range over elements of l-value instance of this.
        static private struct ByLvalueElement(Table)
        {
        pragma(inline, true):
            static if (is(K == class) || isPointer!K) // for reference types
            {
                /// Get reference to front element (key and value).
                @property scope K front()() return @trusted
                {
                    // cast to head-const for class key
                    return cast(typeof(return))_table._bins[_binIndex];
                }
            }
            else
            {
                /// Get reference to front element (key and value).
                @property scope auto ref front() return
                {
                    return _table._bins[_binIndex];
                }
            }
            public LvalueElementRef!(Table) _elementRef;
            alias _elementRef this;
        }

        /// Range over elements of r-value instance of this.
        static private struct ByRvalueElement(Table)
        {
        pragma(inline, true):
            static if (is(K == class) || isPointer!K) // for reference types
            {
                /// Get reference to front element (key and value).
                @property scope K front()() return @trusted
                {
                    // cast to head-const for class key
                    return cast(typeof(return))_table._bins[_binIndex];
                }
            }
            else
            {
                /// Get reference to front element (key and value).
                @property scope auto ref front() return
                {
                    return _table._bins[_binIndex];
                }
            }
            public RvalueElementRef!Table _elementRef;
            alias _elementRef this;
        }
    }

    static if (hasValue)        // HashMap
    {
        scope inout(V)* opBinaryRight(string op)(const scope K key) inout return // `auto ref` here makes things slow
            if (op == "in")
        {
            pragma(inline, true);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(key);
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
        static if (isInstanceOf!(Nullable, K))
        {
            pragma(inline, true)    // LDC must have this
            scope inout(V)* opBinaryRight(string op)(const scope WrappedKey wrappedKey) inout return // `auto ref` here makes things slow
                if (op == "in")
            {
                return opBinaryRight!"in"(K(wrappedKey));
            }
        }

        static private struct ByKey_lvalue(Table)
        {
            pragma(inline, true)
            @property scope const auto ref front() return // key access must be const, TODO auto ref => ref K
            {
                return _table._bins[_binIndex].key;
            }
            public LvalueElementRef!(Table) _elementRef;
            alias _elementRef this;
        }

        static private struct ByKey_rvalue(Table)
        {
            pragma(inline, true)
            @property scope const auto ref front() return // key access must be const, TODO auto ref => ref K
            {
                return _table._bins[_binIndex].key;
            }
            public RvalueElementRef!(Table) _elementRef;
            alias _elementRef this;
        }

        static private struct ByValue_lvalue(Table)
        {
            pragma(inline, true)
            @property scope auto ref front() return @trusted // TODO auto ref => ref V
            {
                return *(cast(ValueType*)&_table._bins[_binIndex].value);
            }
            public LvalueElementRef!(Table) _elementRef;
            alias _elementRef this;
        }

        static private struct ByValue_rvalue(Table)
        {
            pragma(inline, true)
            @property scope auto ref front() return @trusted // TODO auto ref => ref V
            {
                return *(cast(ValueType*)&_table._bins[_binIndex].value);
            }
            public RvalueElementRef!(Table) _elementRef;
            alias _elementRef this;
        }

        /// Key-value element reference with head-const for `class` keys.
        static private struct KeyValueType
        {
            static if (is(K == class) || isPointer!K) // for reference types
            {
                K _key;          // no const because

                /** Key access is head-const. */
                K key() @property @safe pure nothrow @nogc
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

        static private struct ByKeyValue_lvalue(Table)
        {
            pragma(inline, true)
            @property scope auto ref front() return @trusted // TODO auto ref => ref T
            {
                // TODO can this be solved without this `static if`?
                static if (isMutable!(Table))
                {
                    alias E = KeyValueType;
                }
                else
                {
                    alias E = const(T);
                }
                return *(cast(E*)&_table._bins[_binIndex]);
            }
            public LvalueElementRef!(Table) _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys and values of `this`.
        @property scope auto byKeyValue()() return @trusted // template-lazy property
        {
            alias This = MutableThis;
            auto result = ByKeyValue_lvalue!This((LvalueElementRef!(This)(cast(This*)&this)));
            result.findNextNonEmptyBin();
            return result;
        }
        /// ditto
        @property scope auto byKeyValue()() const return @trusted // template-lazy property
        {
            alias This = ConstThis;
            auto result = ByKeyValue_lvalue!This((LvalueElementRef!(This)(cast(This*)&this)));
            result.findNextNonEmptyBin();
            return result;
        }

        /// Indexing.
        scope ref inout(V) opIndex()(const scope K key) inout return // `auto ref` here makes things slow
        {
            version(LDC) pragma(inline, true);
            immutable hitIndex = indexOfKeyOrVacancySkippingHoles(key);
            if (hitIndex != _bins.length &&
                isOccupiedAtIndex(hitIndex))
            {
                return _bins[hitIndex].value;
            }
            else
            {
                import core.exception : RangeError;
                throw new RangeError("key not found");
            }
        }
        static if (isInstanceOf!(Nullable, K))
        {
            scope ref inout(V) opIndex()(const scope WrappedKey wrappedKey) inout return // `auto ref` here makes things slow
            {
                pragma(inline, true);
                return opIndex(K(wrappedKey));
            }
        }

        /** Get value of `key` or `defaultValue` if `key` not present (and
         * therefore `nothrow`).
         *
         * Returns: value reference iff `defaultValue` is an l-value.
         *
         * TODO make `defaultValue` `lazy` when that can be `nothrow`
         */
        auto ref V get()(const scope K key, // template-lazy
                         const scope V defaultValue)
        {
            auto value = key in this;
            if (value !is null)
            {
                return *value;
            }
            else
            {
                return defaultValue;
            }
        }
        static if (isInstanceOf!(Nullable, K))
        {
            auto ref V get()(const scope WrappedKey wrappedKey, // template-lazy
                             const scope V defaultValue)
            {
                pragma(inline, true);
                return get(K(wrappedKey),
                           defaultValue);
            }
        }

	/** Supports $(B aa[key] = value;) syntax.
	 */
        void opIndexAssign()(V value, K key) // template-lazy
	{
            version(LDC) pragma(inline, true);
            insert(T(move(key),
                     move(value)));
            // TODO return reference to value
	}
        static if (isInstanceOf!(Nullable, K))
        {
            void opIndexAssign()(V value, WrappedKey wrappedKey) // template-lazy
            {
                version(LDC) pragma(inline, true);
                insert(T(K(move(wrappedKey)),
                         move(value)));
                // TODO return reference to value
            }
        }
    }

    /** Remove `element`.
     * Returns: `true` if element was removed, `false` otherwise.
     */
    bool remove()(const scope K key) // template-lazy
    {
        version(LDC) pragma(inline, true);
        debug assert(!isBorrowed, borrowedErrorMessage);
        immutable hitIndex = indexOfKeyOrVacancySkippingHoles(key);
        if (hitIndex != _bins.length &&
            isOccupiedAtIndex(hitIndex))
        {
            tagAsLazilyDeletedElementAtIndex(hitIndex);
            _count = _count - 1;
            return true;
        }
        return false;
    }
    static if (isInstanceOf!(Nullable, K))
    {
        bool remove()(const scope WrappedKey wrappedKey) // template-lazy
        {
            pragma(inline, true);
            return remove(K(wrappedKey));
        }
    }

    import traits_ex : isRefIterable;
    import std.range : front;

    /** Remove all elements matching `keys` followed by a rehash.
     *
     * Returns: number of elements that were removed.
     */
    size_t rehashingRemoveN(Keys)(const scope Keys keys) // template-lazy
        if (isRefIterable!Keys &&
            is(typeof(Keys.front == K.init)))
    {
        debug assert(!isBorrowed, borrowedErrorMessage);
        rehash!("!a.isNull && keys.canFind(a)")(); // TODO make this work
        return 0;
    }

    /// Check if empty.
    pragma(inline, true)
    @property bool empty() const { return _count == 0; }

    /// Get length (read-only).
    pragma(inline, true)
    @property size_t length() const { return _count; }

    /// Get bin count.
    pragma(inline, true)
    @property size_t binCount() const { return _bins.length; }

    /** Returns: get total probe count for all elements stored. */
    size_t totalProbeCount()() const // template-lazy
    {
        version(LDC) pragma(inline, true); // LDC needs this or to prevent 10x performance regression in contains()
        static if (hasValue)
        {
            auto range = this.byKeyValue;
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
                alias pred = (const scope element) => (keyOf(element) is keyOf(currentElement));
            }
            else
            {
                alias pred = (const scope auto ref element) => (keyOf(element) is keyOf(currentElement));
            }
            totalCount += triangularProbeCountFromIndex!pred(_bins[], keyToIndex(keyOf(currentElement)));
        }
        return totalCount;
    }

private:
    static if (hasFunctionAttributes!(Allocator.allocate, "@nogc"))
    {
        import container_traits : NoGc;
        @NoGc T[] _bins;        // one element per bin
    }
    else
    {
        T[] _bins;              // one element per bin
    }

    debug                       // use Rust-style borrow checking at run-time
    {
        /// Number of bits needed to store number of read borrows.
        enum borrowCountBits = 24;

        /// Maximum value possible for `_borrowCount`.
        enum borrowCountMax = 2^^borrowCountBits - 1;

        import std.bitmanip : bitfields;
        mixin(bitfields!(size_t, "_count", 8*size_t.sizeof - borrowCountBits,
                         uint, "_borrowCount", borrowCountBits,
                  ));

        pragma(inline, true):
        @safe pure nothrow @nogc:

        @property
        {
            /// Returns: `true` iff `this` is either write or read borrowed.
            bool isBorrowed() const { return _borrowCount >= 1; }

            /// Returns: number of read-only borrowers of `this`.
            uint borrowCount() const { return _borrowCount; }
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

    static if (hasFunctionAttributes!(Allocator.allocate, "@nogc"))
    {
        static if (!hasAddressKey)
        {
            @NoGc size_t* _holesPtr; // bit array describing which bin elements that has been removed (holes)
        }
    }
    else
    {
        static if (!hasAddressKey)
        {
            size_t* _holesPtr; // bit array describing which bin elements that has been removed (holes)
        }
    }

    /** Returns: bin index of `key`. */
    pragma(inline, true)
    private size_t keyToIndex(const scope K key) const
    {
        return hashOf2!(hasher)(key) & powerOf2Mask;
    }

    /** Returns: current index mask from bin count. */
    pragma(inline, true)
    private size_t powerOf2Mask() const
    {
        immutable typeof(return) mask = _bins.length - 1;
        version(internalUnittest) assert((~mask ^ mask) == typeof(mask).max); // isPowerOf2(_bins.length)
        return mask;
    }

    /** Find index to `key` if it exists or to first empty slot found, skipping
     * (ignoring) lazily deleted slots.
     */
    private size_t indexOfKeyOrVacancySkippingHoles(const scope K key) const // `auto ref` here makes things slow
    {
        version(LDC) pragma(inline, true);
        version(internalUnittest)
        {
            assert(!key.isNull);
            static if (hasAddressKey)
            {
                assert(!isHoleKeyConstant(key));
            }
        }
        static if (isCopyable!T)
        {
            /* don't use `auto ref` for copyable `T`'s to prevent
             * massive performance drop for small elements when compiled
             * with LDC. TODO remove when LDC is fixed. */
            static if (hasAddressKey)
            {
                alias pred = (const scope element) => (keyOf(element).isNull ||
                                                       keyOf(element) is key);
            }
            else
            {
                alias pred = (const scope index,
                              const scope element) => (!hasHoleAtPtrIndex(_holesPtr, index) &&
                                                       (keyOf(element).isNull ||
                                                        keyOf(element) is key));
            }
        }
        else
        {
            static if (hasAddressKey)
            {
                alias pred = (const scope auto ref element) => (keyOf(element).isNull ||
                                                                keyOf(element) is key);
            }
            else
            {
                alias pred = (const scope index,
                              const scope auto ref element) => (!hasHoleAtPtrIndex(_holesPtr, index) &&
                                                                (keyOf(element).isNull ||
                                                                 keyOf(element) is key));
            }
        }
        return _bins[].triangularProbeFromIndex!(pred)(keyToIndex(key));
    }

    private size_t indexOfHoleOrNullForKey()(const scope K key) const // template-lazy
    {
        version(LDC) pragma(inline, true);
        version(internalUnittest)
        {
            assert(!key.isNull);
            static if (hasAddressKey)
            {
                assert(!isHoleKeyConstant(key));
            }
        }
        static if (isCopyable!T)
        {
            /* don't use `auto ref` for copyable `T`'s to prevent
             * massive performance drop for small elements when compiled
             * with LDC. TODO remove when LDC is fixed. */
            static if (hasAddressKey)
            {
                alias pred = (const scope element) => (isHoleKeyConstant(keyOf(element)) ||
                                                       keyOf(element).isNull);
            }
            else
            {
                alias pred = (const scope index,
                              const scope element) => (hasHoleAtPtrIndex(_holesPtr, index) ||
                                                       keyOf(element).isNull);
            }
        }
        else
        {
            static if (hasAddressKey)
            {
                alias pred = (const scope auto ref element) => (isHoleKeyConstant(keyOf(element)) ||
                                                                keyOf(element).isNull);
            }
            else
            {
                alias pred = (const scope index,
                              const scope auto ref element) => (hasHoleAtPtrIndex(_holesPtr, index) ||
                                                                keyOf(element).isNull);
            }
        }
        return _bins[].triangularProbeFromIndex!(pred)(keyToIndex(key));
    }

    /** Returns: `true` iff `index` indexes a non-null element, `false`
     * otherwise.
     */
    private bool isOccupiedAtIndex(size_t index) const
    {
        pragma(inline, true);
        version(internalUnittest) assert(index < _bins.length);
        if (keyOf(_bins[index]).isNull) { return false; }
        static if (hasAddressKey)
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
        static assert(0, "cannot duplicate a " ~ T.stringof);
    }
}

/** L-value element reference (and in turn range iterator).
 */
static private struct LvalueElementRef(Table)
{
    import std.traits : Unqual;

    private Table* _table;
    private size_t _binIndex;   // index to bin inside `table`
    private size_t _iterationCounter; // counter over number of elements popped. TODO needed?

    debug
    {
        alias MutableTable = Unqual!(typeof(*_table));
    }

    this(Table* table) @trusted
    {
        pragma(inline, true);
        this._table = table;
        debug
        {
            (cast(MutableTable*)(_table)).incBorrowCount();
        }
    }

    ~this() @trusted
    {
        pragma(inline, true);
        debug
        {
            (cast(MutableTable*)(_table)).decBorrowCount();
        }
    }

    this(this) @trusted
    {
        pragma(inline, true);
        debug
        {
            assert(_table._borrowCount != 0);
            (cast(MutableTable*)(_table)).incBorrowCount();
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
        return _table.length - _iterationCounter;
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
        _iterationCounter += 1;
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
 */
static private struct RvalueElementRef(Table)
{
    Table _table;                // owned table
    size_t _binIndex;            // index to bin inside table
    size_t _iterationCounter;    // counter over number of elements popped

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
        return _table.length - _iterationCounter;
    }

    void popFront()
    {
        version(LDC) pragma(inline, true);
        assert(!empty);
        _binIndex += 1;
        findNextNonEmptyBin();
        _iterationCounter += 1;
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
                  alias Allocator =
                  PureMallocator.instance) = OpenHashMapOrSet!(K, void, hasher, Allocator);

/** Immutable hash map storing keys of type `K` and values of type `V`.
 */
alias OpenHashMap(K,
                  V,
                  alias hasher = hashOf,
                  alias Allocator = PureMallocator.instance) = OpenHashMapOrSet!(K, V, hasher, Allocator);

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

/// `string` as key
@safe pure nothrow @nogc unittest
{
    version(showEntries) dln();
    import digestx.fnv : FNV;
    import container_traits : mustAddGCRange;

    alias E = string;
    alias X = OpenHashMapOrSet!(E, void, FNV!(64, true));
    static assert(!mustAddGCRange!X);
    static assert(X.sizeof == 24); // dynamic arrays also `hasAddressKey`

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

    x.insert("a");
    x.insert("b");
    assert(x.contains("a"));
    assert(x.contains("b"));

    x.remove("a");
    assert(!x.contains("a"));

    x.remove("b");
    assert(!x.contains("b"));

    x.insert("a");
    x.insert("b");
    assert(x.contains("a"));
    assert(x.contains("b"));

    static assert(!__traits(compiles, { testEscapeShouldFail(); } ));
    // TODO this should fail:
    // TODO static assert(!__traits(compiles, { testEscapeShouldFailFront(); } ));
}

/// array container as value type
@safe pure nothrow @nogc unittest
{
    version(showEntries) dln();

    import basic_array : Array = BasicArray;

    alias K = Nullable!(uint, uint.max);

    alias VE = Nullable!(uint, uint.max);
    alias V = OpenHashSet!(VE, FNV!(64, true));

    static assert(!mustAddGCRange!V);

    foreach (X; AliasSeq!(OpenHashMap!(K, V, FNV!(64, true))))
    {
        const VE n = 600;

        auto x = X();

        {                       // scoped range
            auto xkeys = x.byKey;
            assert(xkeys.length == 0);
            foreach (ref key; xkeys) { assert(0); }
            foreach (ref key; X().byKey) { assert(0); }
        }

        foreach (immutable i; 0 .. n)
        {
            assert(x.length == i);

            auto key = K(i);
            auto value = V.withElements([VE(i)].s);

            x[key] = value.dup;
            assert(x.length == i + 1);
            assert(x.contains(key));
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
    alias X = OpenHashMapOrSet!(K, void, FNV!(64, true));

    auto x = X();

    {                           // scoped range
        auto xes = x.byElement;
        assert(xes.length == 0);
        foreach (ref xe; xes) { assert(0); }
    }

    auto x0 = X.init;
    assert(x0.length == 0);
    assert(x0._bins.length == 0);
    assert(!x0.contains(K(1)));

    auto x1 = X.withElements([K(12)].s);
    assert(x1.length == 1);
    assert(x1.contains(12));
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
}

/// r-value and r-value intersection
@safe pure nothrow @nogc unittest
{
    version(showEntries) dln();
    alias K = Nullable!(uint, uint.max);
    alias X = OpenHashMapOrSet!(K, void, FNV!(64, true));

    auto y = X.withElements([K(10), K(12), K(13), K(15)].s).intersectedWith(X.withElements([K(12), K(13)].s));
    assert(y.length == 2);
    assert(y.contains(K(12)));
    assert(y.contains(K(13)));
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
    alias X = OpenHashMapOrSet!(K, void, FNV!(64, true));

    auto x = X.withElements([K(12), K(13)].s);
    auto y = X.withElements([K(10), K(12), K(13), K(15)].s);
    y.intersectWith(x);
    assert(y.length == 2);
    assert(y.contains(K(12)));
    assert(y.contains(K(13)));
}

/** Returns: range that iterates through the elements of `c` in undefined order.
 */
auto byElement(T)(auto ref return inout(T) c) @trusted
    if (isInstanceOf!(OpenHashMapOrSet, T))
{
    alias C = const(T);
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = C.ByLvalueElement!C((LvalueElementRef!(C)(cast(C*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import std.algorithm.mutation : move;
        auto result = C.ByRvalueElement!C((RvalueElementRef!C(move(*(cast(T*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}
alias range = byElement;        // EMSI-container naming

/** Returns: range that iterates through the keys of `c` in undefined order.
 */
auto byKey(T)(auto ref return inout(T) c) @trusted
{
    alias C = const(T);
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = C.ByKey_lvalue!C((LvalueElementRef!(C)(cast(C*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import std.algorithm.mutation : move;
        auto result = C.ByKey_rvalue!C((RvalueElementRef!C(move(*(cast(T*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

/** Returns: range that iterates through the values of `c` in undefined order.
 */
auto byValue(T)(auto ref return inout(T) c) @trusted
{
    alias C = const(T);
    static if (__traits(isRef, c)) // `c` is an l-value and must be borrowed
    {
        auto result = C.ByValue_lvalue!C((LvalueElementRef!(C)(cast(C*)&c)));
    }
    else                        // `c` was is an r-value and can be moved
    {
        import std.algorithm.mutation : move;
        auto result = C.ByValue_rvalue!C((RvalueElementRef!C(move(*(cast(T*)&c))))); // reinterpret
    }
    result.findNextNonEmptyBin();
    return result;
}

/// make range from l-value and r-value. element access is always const
@system pure unittest
{
    version(showEntries) dln();
    alias K = Nullable!(uint, uint.max);
    alias X = OpenHashMapOrSet!(K, void, FNV!(64, true));

    auto k11 = K(11);
    auto k22 = K(22);
    auto k33 = K(33);
    immutable ks = [k11, k22, k33].s;
    auto k44 = K(44);

    // mutable
    auto x = X.withElements(ks);
    assert(!x.contains(k44));
    assert(x.length == 3);
    assert(x.byElement.count == x.length);
    foreach (e; x.byElement)    // from l-value
    {
        static assert(is(typeof(e) == const(K))); // always const access

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

        const eHit = e in x;
        assert(eHit);           // found
        assert(*eHit is e);     // and the value equals what we searched for

        const eDup = x.dup;     // duplication is `const` and allowed
    }

    // const
    const y = X.withElements(ks);
    assert(!x.contains(k44));
    foreach (e; y.byElement)    // from l-value
    {
        auto z = y.byElement;   // ok to read-borrow again
        assert(y.contains(e));
        static assert(is(typeof(e) == const(K)));
    }

    foreach (e; X.withElements([K(11)].s).byElement) // from r-value
    {
        assert(e == K(11));
        static assert(is(typeof(e) == const(K))); // always const access
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
    static assert(is(typeof(vp) == V*));
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
    static assert(is(typeof(vp) == V*));

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

    foreach (e; x.byKey)
    {
        static assert(is(typeof(e) == const(X.KeyType)));
    }

    foreach (e; x.byValue)
    {
        static assert(is(typeof(e) == X.ValueType)); // TODO should be const(X.ValueType)
    }

    foreach (e; x.byKeyValue)
    {
        static assert(is(typeof(e.key) == const(X.KeyType)));
        static assert(is(typeof(e.value) == const(X.ValueType)));
        static assert(is(typeof(e) == const(X.ElementType)));
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
    alias K = Nullable!(S, S(uint.min)); // use uint.min to trigger use of faster `Allocator.zeroallocate`

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
        static assert(is(typeof(e) == X.ValueType)); // mutable access to value
        assert(e.data == 43);

        // value mutation side effects
        e.data += 1;
        assert(e.data == 44);
        e.data -= 1;
        assert(e.data == 43);
    }

    foreach (ref e; x.byKeyValue)   // `e` is auto ref
    {
        static assert(is(typeof(e.key) == const(X.KeyType))); // const access to key
        static assert(is(typeof(e.value) == X.ValueType)); // mutable access to value

        assert(e.key.value == 42);
        assert(e.value.data == 43);

        // key cannot be mutated
        static assert(!__traits(compiles, { e.key.value += 1; }));

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
        static assert(is(typeof(e) == X.ValueType)); // mutable access to value
        assert(e.data == 43);

        // value mutation side effects
        e.data += 1;
        assert(e.data == 44);
        e.data -= 1;
        assert(e.data == 43);
    }

    foreach (ref e; x.byKeyValue)   // `e` is auto ref
    {
        static assert(is(typeof(e.key) == X.KeyType)); // mutable access to class key
        static assert(is(typeof(e.value) == X.ValueType)); // mutable access to value

        assert(e.key.value == 42);
        assert(e.value.data == 43);

        // class key itself should not be mutable
        static assert(!__traits(compiles, { e.key = null; }));

        // members of key can be mutated
        static assert(__traits(compiles, { e.key.value += 1; }));

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
        static assert(is(typeof(e) == X.ValueType)); // mutable access to value
        assert(e.data == 43);

        // value mutation side effects
        e.data += 1;
        assert(e.data == 44);
        e.data -= 1;
        assert(e.data == 43);
    }

    foreach (ref e; x.byKeyValue) // `e` is auto ref
    {
        static assert(is(typeof(e.key) == X.KeyType)); // mutable access to class key
        static assert(is(typeof(e.value) == X.ValueType)); // mutable access to value

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
    const n = 600;

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

        static assert(!__traits(compiles, { const _ = x < y; })); // no ordering

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
            sink.formattedWrite(typeof(this).stringof,
                                "(%s)", _value);
        }

        private ulong _value;
    }

    import container_traits : mustAddGCRange;
    static assert(mustAddGCRange!string);

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
                static assert(is(typeof(R.init) == R));
                static assert(is(ReturnType!((R xr) => xr.empty) == bool));

                static assert(!__traits(compiles, { xr.front == K.init; })); // always head-const
                auto f = xr.front;
                static if (is(K == class))
                {
                    static assert(is(typeof(f) == K)); // tail-mutable
                }
                else
                {
                    static assert(is(typeof(f) == const(K))); // tail-const
                }

                static assert(is(typeof((R xr) => xr.front)));
                static assert(!is(ReturnType!((R xr) => xr.front) == void));
                static assert(is(typeof((R xr) => xr.popFront)));

                static assert(isInputRange!(typeof(xr)));

                assert(x.byElement.count == 3);

                X y;
                size_t ix = 0;
                foreach (ref e; x.byElement)
                {
                    assert(x.contains(e));
                    assert(!y.contains(e));
                    static if (is(K == class))
                    {
                        y.insert(cast(K)e); // ugly but ok in tests
                    }
                    else
                    {
                        y.insert(e);
                    }
                    assert(y.contains(e));
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

                    // TODO http://forum.dlang.org/post/kvwrktmameivubnaifdx@forum.dlang.org
                    xc.removeAllMatching!(_ => _ == k11);

                    assert(xc.length == 2);
                    assert(!xc.contains(k11));

                    xc.removeAllMatching!(_ => _ == k12);
                    assert(!xc.contains(k12));
                    assert(xc.length == 1);

                    xc.removeAllMatching!(_ => _ == k13);
                    assert(!xc.contains(k13));
                    assert(xc.length == 0);

                    // this is ok
                    foreach (e; xc.byElement) {}
                }

                {               // ByRvalueElement
                    auto k = X.withElements([k11, k12].s).filtered!(_ => _ != k11).byElement;
                    static assert(isInputRange!(typeof(k)));
                    assert(k.front == k12);

                    static assert(!__traits(compiles, { k.front = K.init; })); // head-const
                    static if (is(K == class))
                    {
                        static assert(is(typeof(k.front) == K)); // tail-mutable
                    }
                    else
                    {
                        static assert(is(typeof(k.front) == const(K))); // tail-const
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
                    }
                    q.clear();
                    assert(q.empty);
                }
            }

            import container_traits : mustAddGCRange;
            static if (is(V == string))
            {
                static assert(mustAddGCRange!V);
                static assert(mustAddGCRange!(V[1]));
                static assert(mustAddGCRange!(X.T));
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

    enum Alts { a, b, c, d }

    struct Zingrel
    {
        Zing zing;
        Alts alts;

        @safe pure nothrow @nogc pragma(inline, true):
        bool isNull() const { return zing is null; }
        void nullify() { zing = null; }
        enum nullValue = typeof(this).init;
    }
    static assert(isNullable!Zingrel);

    alias X = OpenHashMapOrSet!(Zingrel, void, FNV!(64, true));
    X x;

    auto e = Zingrel(new Zing(42), Alts.init);

    assert(!x.contains(e));
    assert(x.insert(e) == X.InsertionStatus.added);
    assert(x.contains(e));
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
