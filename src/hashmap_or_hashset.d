module hashmap_or_hashset;

import container_traits;

/** Insertion status.
 */
enum InsertionStatus
{
    added,                      // element was added
    modified,                   // value of element was changed (map only). TODO only for Map-case
    unmodified                  // element was left unchanged
}

/** Hash set (or map) storing (key) elements of type `K` and values of type `V`.
 *
 * Uses small-size-optimized (SSO) arrays as bins, which provides more stable
 * behaviour than open-addressing.
 *
 * Params:
 *      K = key type.
 *      V = value type.
 *      Allocator = memory allocator for bin array and large bins (`LargeBin`)
 *      hasher = hash function or std.digest Hash.
 *      smallBinMinCapacity = minimum capacity of small bin
 *
 * See also: https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/
 *
 * TODO try quadratic probing using triangular numbers:
 * http://stackoverflow.com/questions/2348187/moving-from-linear-probing-to-quadratic-probing-hash-collisons/2349774#2349774
 *
 * TODO in non-sso optimized store add flag similar to Nullable that reserves a
 * specific value for key that indicates that slot is unused. Use this when
 * storing indexes in knet.storage
 *
 * TODO support uncopyable keys
 *
 * TODO add extractElement that moves it out similar to
 * http://en.cppreference.com/w/cpp/container/unordered_set/extract
 *
 * TODO add merge or union algorithm here or into container_algorithm.d. See
 * also: http://en.cppreference.com/w/cpp/container/unordered_set/merge
 *
 * TODO add test for `SetIntersectionFast` in setops_ex.d
 *
 * TODO adjust rehashing to occur when relative number of LargeBuckets is
 * larger than, say, 1/10. Experiment with different ratios.
 *
 * TODO add template parameter `alias nullKeyValue` that avoids having to store
 * `bstates` when smallBinCapacity == 1, similar to:
 *     std.typecons.nullable(alias nullValue, T)( T t )
 *
 * TODO add flag for use growth factor smaller than powers of two. use prime_modulo.d
 *
 * TODO use core.bitop : bsr, bsl to find first empty element in bin. if as fast
 * as current find use it to optimize remove()
 *
 * TODO Avoid extra length and capacity in _statuses (length or large) by making
 * it allocate in sync with bins (using soa.d).
 *
 * TODO also try allocating values in a separate array using soa.d and see if
 * benchmarks become better
 *
 * TODO growWithExtraCapacity(): if allocator has realloc we can do rehashing in-place similar to
 * reordering in in-place radix (integer_sorting.d), otherwise rehash into new
 * copy of bins and free old bins when done. If bin element count is >
 * 1 this is more complicated since each bin contains a set of elements to
 * swap out and must be put in a queue.
 *
 * TODO benchmark against https://github.com/greg7mdp/sparsepp
 */
struct HashMapOrSet(K, V = void,
                    alias Allocator = null,
                    alias hasher = hashOf,
                    uint smallBinMinCapacity = 1,
                    uint capacityScaleNumerator = 2,
                    uint capacityScaleDenominator = 1)
    if (// isHashable!K &&
        smallBinMinCapacity >= 1) // no use having empty small bins
{
    import std.conv : emplace;
    import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor, isCopyable, isMutable;
    import std.meta : Unqual;
    import std.algorithm.comparison : max;
    import std.algorithm.mutation : move, moveEmplace;
    import emplace_all : moveEmplaceAllNoReset;
    // TODO activate and use import prime_modulo;

    /** In the hash map case, `V` is non-void, and a value is stored alongside
     * the key of type `K`.
     */
    enum hasValue = !is(V == void);

    alias MutableThis = Unqual!(typeof(this));
    alias ConstThis = const(MutableThis);

    pragma(inline):

    /// Element type.
    static if (hasValue)
    {
        struct T
        {
            K key;
            V value;
        }

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            return element.key;
        }

        /// Get reference to key part of `element`.
        static ref inout(K) keyRefOf()(ref return inout(T) element) // template-lazy
        {
            return element.key;
        }

        /// Get value part of element.
        static auto ref inout(V) valueOf()(auto ref return inout(T) element)
        {
            return element.value;
        }

        /** Type of key stored. */
        alias KeyType = K;

        /** Type of value stored. */
        alias ValueType = V;

        enum keyEqualPred = "a.key is b";
    }
    else                        // HashSet
    {
        alias T = K;

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            return element;
        }

        /// Get reference to key part of `element`.
        static ref inout(K) keyRefOf()(ref return inout(T) element) // template-lazy
        {
            return element;
        }

        enum keyEqualPred = "a is b";
    }

    alias ElementType = T;

    /** Make with room for storing at least `capacity` number of elements.
     */
    pragma(inline)              // LDC can, DMD cannot inline
    static typeof(this) withCapacity()(size_t capacity) // template-lazy
    {
        return typeof(return)(capacity);
    }

    import std.traits : isIterable;

    /** Make with `elements`. */
    static typeof(this) withElements(R)(R elements)
        if (isIterable!R)
    {
        import std.range : hasLength;
        static if (hasLength!R)
        {
            typeof(this) that = withCapacity(elements.length);
        }
        else
        {
            typeof(this) that;  // TODO if `isForwardRange` count elements
        }
        foreach (ref element; elements)
        {
            that.insert(element);
        }
        return that;
    }

    pragma(inline)              // LDC can, DMD cannot inline
    private static typeof(this) withBinCount()(size_t binCount) // template-lazy
    {
        typeof(return) that;    // TODO return direct call to store constructor
        that._bins = Bins.withLength(binCount);
        that._bstates = Bstates.withLength(binCount);
        that._length = 0;
        return that;
    }

    /** Construct with room for storing at least `capacity` number of elements.
     */
    private this()(size_t capacity) // template-lazy
    {
        immutable binCount = binCountOfCapacity(capacity);
        _bins = Bins.withLength(binCount);
        _bstates = Bstates.withLength(binCount);
        _length = 0;
    }

    /** Lookup bin count from capacity `capacity`.
     */
    static private size_t binCountOfCapacity()(size_t capacity) // template-lazy
    {
        const minimumBinCount = ((capacityScaleNumerator *
                                  capacity) /
                                 (smallBinCapacity *
                                  capacityScaleDenominator));
        import std.math : nextPow2;
        return nextPow2(minimumBinCount == 0 ?
                        0 :
                        minimumBinCount - 1);
    }

    /// Destruct.
    ~this()
    {
        release();
    }

    /// No copying.
    @disable this(this);

    /// Duplicate.
    static if (isCopyable!T)
    {
        typeof(this) dup()() @trusted // template-lazy
        {
            typeof(return) that;

            that._bins.reserve(_bins.length);
            // TODO merge these
            that._bins.length = _bins.length; // TODO this zero-initializes before initialization below, use unsafe setLengthOnlyUNSAFE

            that._bstates = _bstates.dup;
            assert(that._bstates[] == _bstates[]);

            that._length = _length;

            foreach (immutable binIx; 0 .. _bins.length)
            {
                if (_bstates[binIx].isLarge)
                {
                    LargeBin.emplaceWithCopiedElements(&that._bins[binIx].large,
                                                       _bins[binIx].large[]);
                }
                else
                {
                    auto elements = smallBinElementsAt(binIx);
                    /** TODO functionize to `emplaceAll` in emplace_all.d. See also:
                     * http://forum.dlang.org/post/xxigbqqflzwfgycrclyq@forum.dlang.org
                     */
                    static if (hasElaborateCopyConstructor!T)
                    {
                        foreach (immutable elementIx, immutable ref element; elements)
                        {
                            emplace(&that._bins[binIx].small[elementIx],
                                    element);
                        }
                    }
                    else
                    {
                        enum useMemcpy = true;
                        static if (useMemcpy)
                        {
                            // currently faster than slice assignment on else branch
                            import core.stdc.string : memcpy;
                            memcpy(that._bins[binIx].small.ptr, // cannot overlap
                                   elements.ptr,
                                   elements.length * T.sizeof);
                        }
                        else
                        {
                            that._bins[binIx].small.ptr[0 .. _bstates[binIx].smallCount] = elements;
                        }
                    }
                }
            }

            return that;
        }
    }

    /// Grow by duplicating number of bins.
    private void growWithExtraCapacity(size_t extraCapacity) @trusted // not template-lazy
    {
        size_t newBinCount = 0;
        if (extraCapacity == 1)
        {
            newBinCount = binCount ? 2 * binCount : 1; // 0 => 1, 1 => 2, 2 => 4, ...
        }
        else
        {
            newBinCount = binCountOfCapacity(_length + extraCapacity);
        }
        auto copy = withBinCount(newBinCount);

        // move elements to copy
        foreach (immutable binIx; 0 .. _bins.length)
        {
            foreach (ref element; binElementsAt(binIx))
            {
                copy.insertMoveWithoutBinCountGrowth(element);
            }
        }
        assert(copy._length == _length); // length shouldn't change

        moveEmplace(copy._bstates, _bstates); // `_bstates` doesn't need destroying
        move(copy._bins, _bins);

        assert(!_bins.empty);
        assert(!_bstates.empty);
    }

    /// Equality.
    bool opEquals()(in auto ref typeof(this) rhs) const @trusted
    {
        if (_length != rhs._length) { return false; }

        foreach (immutable binIx; 0 .. _bins.length)
        {
            foreach (const ref element; binElementsAt(binIx))
            {
                static if (hasValue)
                {
                    auto elementFound = element.key in rhs;
                    if (!elementFound) { return false; }
                    if ((*elementFound) !is element.value) { return false; }
                }
                else
                {
                    if (!rhs.contains(element)) { return false; }
                }
            }
        }

        return true;
    }

    /// Empty.
    void clear()()              // template-lazy
    {
        release();
        resetInternalData();
    }

    /// Release internal store.
    private void release() @trusted
    {
        foreach (immutable binIx; 0 .. _bins.length)
        {
            if (_bstates[binIx].isLarge)
            {
                static if (hasElaborateDestructor!LargeBin)
                {
                    .destroy(_bins[binIx].large);
                }
            }
            else
            {
                static if (hasElaborateDestructor!T)
                {
                    // TODO use emplace_all : destroyAll(smallBinElementsAt(binIx))
                    foreach (ref element; smallBinElementsAt(binIx))
                    {
                        .destroy(element);
                    }
                }
            }
        }
    }

    /// Reset internal data.
    private void resetInternalData()
    {
        _bins.clear();
        _bstates.clear();
        _length = 0;
    }

    version(LDC) { pragma(inline, true): } // needed for LDC to inline this, DMD cannot
    pragma(inline, true):                  // LDC must have this

    /** Check if `element` is stored.
        Returns: `true` if element was already present, `false` otherwise.
     */
    bool contains()(in K key) const @trusted // template-lazy. TODO make `auto ref K` work
    {
        if (empty)              // TODO can this check be avoided?
        {
            return false; // prevent `RangeError` in `binElementsAt` when empty
        }
        immutable binIx = keyToBinIx(key);
        return hasKey(binElementsAt(binIx), key);
    }
    /// ditto
    bool contains()(in ref K key) const @trusted // template-lazy
    {
        if (empty)              // TODO can this check be avoided?
        {
            return false; // prevent `RangeError` in `binElementsAt` when empty
        }
        immutable binIx = keyToBinIx(key);
        return hasKey(binElementsAt(binIx), key);
    }

    /** Insert `element`, being either a key, value (map-case) or a just a key (set-case).
     */
    pragma(inline, true)
    InsertionStatus insert(T element)
    {
        reserveExtra(1);
        return insertMoveWithoutBinCountGrowth(element);
    }

    /** Reserve rom for `extraCapacity` number of extra buckets. */
    void reserveExtra()(size_t extraCapacity)
    {
        if ((capacityScaleNumerator *
             (_length + extraCapacity) /
             capacityScaleDenominator) >
            _bins.length * smallBinCapacity)
        {
            growWithExtraCapacity(extraCapacity);
        }
    }

    static if (hasValue)
    {
        /** Insert or replace `value` at `key`. */
        pragma(inline, true)    // LDC must have this
        InsertionStatus insert(K key, V value)
        {
            return insert(T(move(key),
                            move(value)));
        }
    }

    pragma(inline, true)              // DMD cannot inline
    static private size_t offsetOfKey(in T[] elements,
                                      in ref K key)
    {
        size_t elementOffset = 0;
        foreach (const ref e; elements)
        {
            if (keyOf(e) is key) { break; }
            elementOffset += 1;
        }
        return elementOffset;
    }

    pragma(inline, true)
    static private bool hasKey(in T[] elements,
                               in ref K key)
    {
        return offsetOfKey(elements, key) != elements.length;
    }

    /** Insert `element` like with `insert()` without automatic growth of number
     * of bins.
     */
    InsertionStatus insertMoveWithoutBinCountGrowth(ref T element) @trusted // ref simplifies move
    {
        immutable binIx = keyToBinIx(keyRefOf(element));
        T[] elements = binElementsAt(binIx);
        immutable elementOffset = offsetOfKey(elements, keyOf(element));
        immutable elementFound = elementOffset != elements.length;

        if (elementFound)
        {
            static if (hasValue)
            {
                /* TODO Rust does the same in its `insert()` at
                 * https://doc.rust-lang.org/std/collections/struct.HashMap.html
                 */
                if (elements[elementOffset].value !is valueOf(element)) // if different value (or identity for classes)
                {
                    // replace value
                    static if (needsMove!V)
                    {
                        move(valueOf(element),
                             elements[elementOffset].value);
                    }
                    else
                    {
                        elements[elementOffset].value = valueOf(element);
                    }
                    return typeof(return).modified;
                }
            }
            return typeof(return).unmodified;
        }
        else                    // no elementFound
        {
            if (_bstates[binIx].isLarge) // stay large
            {
                _bins[binIx].large.insertBackMove(element);
            }
            else
            {
                if (_bstates[binIx].isFullSmall) // expand small to large
                {
                    static if (needsMove!T)
                    {
                        // TODO functionize to concatenation:moveConcatenate()
                        T[smallBinCapacity + 1] smallCopy = void;
                        moveEmplaceAllNoReset(_bins[binIx].small[],
                                              smallCopy[0 .. smallBinCapacity]);
                        moveEmplace(element,
                                    smallCopy[smallBinCapacity]);

                        LargeBin.emplaceWithMovedElements(&_bins[binIx].large,
                                                          smallCopy[]);
                    }
                    else
                    {
                        import concatenation : concatenate;
                        auto smallCopy = concatenate(_bins[binIx].small, element);
                        emplace!(LargeBin)(&_bins[binIx].large, smallCopy[]);
                    }
                    _bstates[binIx].makeLarge();
                }
                else            // stay small
                {
                    static if (needsMove!T)
                    {
                        moveEmplace(element,
                                    _bins[binIx].small[_bstates[binIx].smallCount]);
                    }
                    else
                    {
                        _bins[binIx].small[_bstates[binIx].smallCount] = element;
                    }
                    _bstates[binIx].incSmallCount();
                }
            }
            _length += 1;
            return typeof(return).added;
        }
    }

    /** Element reference (and in turn range iterator).
     */
    static private struct ElementRef(HashMapOrSetType)
    {
        HashMapOrSetType* table;
        size_t binIx;           // index to bin inside table
        size_t elementOffset;   // offset to element inside bin

        pragma(inline, true):

        /// Check if empty.
        @property bool empty() const
        {
            return binIx == table.binCount;
        }

        void initFirstNonEmptyBin()
        {
            while (binIx < table.binCount &&
                   table.binElementCountAt(binIx) == 0)
            {
                binIx += 1;
            }
        }

        pragma(inline)
        void popFront()
        {
            assert(!empty);
            elementOffset += 1; // next element
            // if current bin was emptied
            while (elementOffset >= table.binElementsAt(binIx).length)
            {
                // next bin
                binIx += 1;
                elementOffset = 0;
                if (empty) { break; }
            }
        }

        @property typeof(this) save() // ForwardRange
        {
            return this;
        }
    }

    static if (!hasValue)       // HashSet
    {
        pragma(inline, true)
        bool opBinaryRight(string op)(in K key) inout @trusted
            if (op == "in")
        {
            return contains(key);
        }

        static private struct ByElement(HashMapOrSetType)
        {
        pragma(inline, true):

            static if (is(ElementType == class))
            {
                /// Get reference to front element (key and value).
                @property scope auto front()() return @trusted
                {
                    /* cast away const from `HashMapOrSetType` for classes
                     * because class elements are currently hashed and compared
                     * compared using their identity (pointer value) `is`
                     */
                    return cast(ElementType)table.binElementsAt(binIx)[elementOffset];
                }
            }
            else
            {
                /// Get reference to front element (key and value).
                @property scope auto front()()
                {
                    return table.binElementsAt(binIx)[elementOffset];
                }
            }

            public ElementRef!HashMapOrSetType _elementRef;

            alias _elementRef this;
        }

        /// Returns forward range that iterates through the values of `this`.
        @property auto byElement()() inout // template-lazy
        {
            static if (isMutable!(typeof(this)))
            {
                alias This = MutableThis;
            }
            else
            {
                alias This = ConstThis;
            }
            auto result = ByElement!This((ElementRef!This(cast(This*)&this)));
            result.initFirstNonEmptyBin();
            return result;
        }
        /// ditto
        pragma(inline, true)
        scope auto opSlice()() inout return // template-lazy
        {
            return byElement();
        }
    }

    static if (hasValue)        // HashMap
    {
        scope inout(V)* opBinaryRight(string op)(in K key) inout @trusted return
            if (op == "in")
        {
            if (empty)
            {
                // prevent range error in `binElementsAt` when `this` is empty
                return typeof(return).init;
            }
            immutable binIx = keyToBinIx(key);
            const elements = binElementsAt(binIx);
            immutable elementOffset = offsetOfKey(elements, key);
            immutable elementFound = elementOffset != elements.length;
            if (elementFound)
            {
                return cast(typeof(return))&elements[elementOffset].value;
                // return typeof(return)(&this, binIx, elementOffset);
            }
            else                    // miss
            {
                return typeof(return).init;
            }
        }

        static private struct ByKey(HashMapOrSetType)
        {
            pragma(inline, true):
            /// Get reference to key of front element.
            @property scope const auto ref front()() return // key access must be const
            {
                return table.binElementsAt(binIx)[elementOffset].key;
            }
            public ElementRef!HashMapOrSetType _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys of `this`.
        @property scope auto byKey()() inout return // template-lazy property
        {
            static if (isMutable!(typeof(this)))
            {
                alias This = MutableThis;
            }
            else
            {
                alias This = ConstThis;
            }
            auto result = ByKey!This((ElementRef!This(cast(This*)&this)));
            result.initFirstNonEmptyBin();
            return result;
        }

        static private struct ByValue(HashMapOrSetType)
        {
            pragma(inline, true):
            /// Get reference to value of front element.
            @property scope auto ref front()() return // template-lazy property
            {
                return table.binElementsAt(binIx)[elementOffset].value;
            }
            public ElementRef!HashMapOrSetType _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the values of `this`.
        @property scope auto byValue()() inout return // template-lazy property
        {
            static if (isMutable!(typeof(this)))
            {
                alias This = MutableThis;
            }
            else
            {
                alias This = ConstThis;
            }
            auto result = ByValue!This((ElementRef!This(cast(This*)&this)));
            result.initFirstNonEmptyBin();
            return result;
        }

        static private struct ByKeyValue(HashMapOrSetType)
        {
            pragma(inline, true):
            /// Get reference to front element (key and value).
            @property scope auto ref front()() return
            {
                return table.binElementsAt(binIx)[elementOffset];
            }
            public ElementRef!HashMapOrSetType _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys and values of `this`.
        @property scope auto byKeyValue()() inout return // template-lazy property
        {
            static if (isMutable!(typeof(this)))
            {
                alias This = MutableThis;
            }
            else
            {
                alias This = ConstThis;
            }
            auto result = ByKeyValue!This((ElementRef!This(cast(This*)&this)));
            result.initFirstNonEmptyBin();
            return result;
        }
        /// ditto
        pragma(inline, true)
        scope auto opSlice()() return  // template-lazy
        {
            return byKeyValue();
        }

        /// Indexing.
        pragma(inline, true)    // LDC must have this
        scope ref inout(V) opIndex()(in auto ref K key) inout return
        {
            immutable binIx = keyToBinIx(key);
            auto elements = binElementsAt(binIx);

            immutable elementOffset = offsetOfKey(elements, key);
            immutable elementFound = elementOffset != elements.length;
            if (elementFound)
            {
                return binElementsAt(binIx)[elementOffset].value;
            }
            else                // miss
            {
                import core.exception : RangeError;
                throw new RangeError("Key not in table");
            }
        }

        /** Get value of `key` or `defaultValue` if `key` not present (and
         * therefore `nothrow`).
         *
         * Returns: value reference iff `defaultValue` is an l-value.
         *
         * TODO make `defaultValue` `lazy` when that can be `nothrow`
         */
        auto ref V get()(in K key, in auto ref V defaultValue) @trusted
        {
            import std.algorithm.searching : countUntil;
            immutable binIx = keyToBinIx(key);
            immutable ptrdiff_t elementOffset = binElementsAt(binIx).countUntil!(_ => _.key is key); // TODO functionize
            if (elementOffset != -1) // elementFound
            {
                return binElementsAt(binIx)[elementOffset].value;
            }
            else                    // miss
            {
                return defaultValue;
            }
        }

	/** Supports $(B aa[key] = value;) syntax.
	 */
        pragma(inline, true)
        void opIndexAssign()(V value, K key) // template-lazy
	{
            insert(T(move(key),
                     move(value)));
            // TODO return reference to value
	}

        static if (__traits(compiles, { V _; _ += 1; })) // if we can increase the key
        {
            /** Increase value at `key`, or set value to 1 if `key` hasn't yet
             * been added.
             */
            pragma(inline, true)
            void autoinitIncAt()(in K key) // template-lazy
            {
                auto elementFound = key in this;
                if (elementFound)
                {
                    (*elementFound) += 1;
                }
                else
                {
                    insert(key, V.init + 1);
                }
            }
        }

    }

    /** Remove `element` and, when possible, shrink its large bin to small.

        Returns: `true` if element was removed, `false` otherwise.
    */
    bool remove()(in K key)     // template-lazy
        @trusted
    {
        immutable binIx = keyToBinIx(key);
        import container_algorithm : popFirstMaybe;
        if (_bstates[binIx].isLarge)
        {
            immutable elementFound = _bins[binIx].large.popFirstMaybe!keyEqualPred(key);
            _length -= elementFound ? 1 : 0;
            if (elementFound)
            {
                tryShrinkLargeBinAt(binIx);
            }
            return elementFound;
        }
        else
        {
            const elements = smallBinElementsAt(binIx);
            immutable elementIx = offsetOfKey(elements, key);
            immutable elementFound = elementIx != elements.length;
            if (elementFound)
            {
                removeSmallElementAt(binIx, elementIx);
            }
            _length -= elementFound ? 1 : 0;
            return elementFound;
        }
    }

    /** Remove small element at `elementIx` in bin `binIx`. */
    private void removeSmallElementAt()(size_t binIx, // template-lazy
                                        size_t elementIx)
    {
        assert(!_bstates[binIx].isLarge);
        import container_algorithm : shiftToFrontAt;
        smallBinElementsAt(binIx).shiftToFrontAt(elementIx);
        _bstates[binIx].decSmallCount();
        static if (hasElaborateDestructor!T)
        {
            .destroy(_bins[binIx].small[_bstates[binIx].smallCount]);
        }
    }

    /** Shrink large bin at `binIx` possible posbiel. */
    private void tryShrinkLargeBinAt()(size_t binIx) // template-lazy
    {
        assert(_bstates[binIx].isLarge);
        immutable count = _bins[binIx].large.length;
        if (count <= smallBinCapacity) // large fits in small
        {
            SmallBin smallCopy = void;
            moveEmplaceAllNoReset(_bins[binIx].large[0 .. count],
                                  smallCopy[0 .. count]);
            static if (hasElaborateDestructor!LargeBin)
            {
                .destroy(_bins[binIx].large);
            }
            moveEmplaceAllNoReset(smallCopy[0 .. count],
                                  _bins[binIx].small[0 .. count]);
            _bstates[binIx].smallCount = count;
        }
    }

    /** Rehash.
     *
     * Reorganize `this` in place so that lookups are more efficient.
     */
    ref typeof(this) rehash()() @trusted // template-lazy
    {
        static assert(false, "TODO remove template parens of this functions and implement");
        // return this;
    }

    /// Check if empty.
    pragma(inline, true)
    @property bool empty() const { return _length == 0; }

    /// Get length (read-only).
    pragma(inline, true)
    @property size_t length() const { return _length; }

    /// Get bin count.
    pragma(inline, true)
    @property size_t binCount() const { return _bins.length; }

    /// Bin count statistics.
    struct BinCounts
    {
        size_t smallCount;      // number of hybrid bins being small
        size_t largeCount;      // number of hybrid bins being large
    }

    /// Returns: bin count statistics for small and large bins.
    pragma(inline, false)
    BinCounts binCounts()() const // template-lazy
    {
        import std.algorithm : count;
        immutable largeCount = _bstates[].count!(_ => _.isLarge);
        immutable smallCount = _bstates.length - largeCount;
        auto result = typeof(return)(smallCount,
                                     largeCount);
        assert(result.largeCount + result.smallCount == _bstates.length);
        return result;
    }

    /** Returns: elements in bin at `binIx`. */
    pragma(inline, true)
    private scope inout(T)[] binElementsAt(size_t binIx) inout return @trusted
    {
        if (_bstates[binIx].isLarge)
        {
            return _bins[binIx].large[];
        }
        else
        {
            return smallBinElementsAt(binIx);
        }
    }

    pragma(inline, true)
    private scope inout(T)[] smallBinElementsAt(size_t binIx) inout return
    {
        return _bins[binIx].small[0 .. _bstates[binIx].smallCount];
    }

    /** Returns: number of elements in bin at `binIx`. */
    pragma(inline, true)
    private size_t binElementCountAt(size_t binIx) const @trusted
    {
        if (_bstates[binIx].isLarge)
        {
            return _bins[binIx].large.length;
        }
        else
        {
            return _bstates[binIx].smallCount;
        }
    }

    /** Maximum number of elements that fits in a small bin
     * (`SmallBin`).
     */
    enum smallBinCapacity = max(smallBinMinCapacity,
                                   LargeBin.sizeof / T.sizeof);

private:
    import basic_array : Array = BasicArray;

    /** 32-bit capacity and length for LargeBinLnegth on 64-bit platforms
     * saves one word and makes insert() and contains() significantly faster */
    alias LargeBinCapacityType = uint;
    alias LargeBin = Array!(T, Allocator, LargeBinCapacityType);

    alias SmallBin = T[smallBinCapacity];

    /// no space for `SmallBin`s should be wasted
    static assert(SmallBin.sizeof >= LargeBin.sizeof);

    /** Small-size-optimized bin.
     */
    union HybridBin
    {
        SmallBin small;
        LargeBin large;
    }

    static if (mustAddGCRange!LargeBin)
    {
        static assert(mustAddGCRange!HybridBin, "HybridBin mustAddGCRange when LargeBin is " ~ LargeBin.stringof);
    }
    static if (mustAddGCRange!SmallBin)
    {
        static assert(mustAddGCRange!HybridBin, "HybridBin mustAddGCRange when SmallBin is " ~ SmallBin.stringof);
    }

    /** Count and large status of bin. */
    struct Bstate
    {
        alias Count = ubyte;

        pragma(inline, true):

        @property Count smallCount() const
        {
            assert(_count <= smallBinCapacity);
            return _count;
        }

        @property void smallCount(size_t count)
        {
            assert(count <= smallBinCapacity);
            _count = cast(Count)count;
        }

        void decSmallCount()
        {
            assert(_count >= 1);
            _count -= 1;
        }

        void incSmallCount()
        {
            assert(_count + 1 <= smallBinCapacity);
            _count += 1;
        }

        /** Returns: `true` iff `this` is a large bin. */
        @property bool isLarge() const
        {
            return _count == Count.max;
        }

        void makeLarge()
        {
            _count = Count.max;
        }

        /** Returns: `true` iff `this` is a full small bin. */
        @property bool isFullSmall() const
        {
            return _count == smallBinCapacity;
        }

        Count _count;
    }

    /** Small-size-optimized bin array.
        Size-state (including small or large) for each element is determined by
        corresponding element in `Bstates`.
     */
    alias Bins = Array!(HybridBin, Allocator);

    /// Bin states.
    alias Bstates = Array!(Bstate, Allocator);

    // TODO merge these into an instance of soa.d and remove invariant
    Bins _bins;                 // bin elements
    Bstates _bstates;           // bin states
    invariant
    {
        assert(_bins.length ==
               _bstates.length);
    }

    size_t _length;             // total number of elements stored

    /** Returns: bin index of `hash`. */
    pragma(inline, true)
    size_t hashToIndex(hash_t hash) const
    {
        const size_t mask = _bins.length - 1;
        assert((~mask ^ mask) == size_t.max); // isPowerOf2(_bins.length)
        return hash & mask;
    }

    /** Returns: bin index of `key`. */
    pragma(inline, true)
    size_t keyToBinIx()(in auto ref K key) const
    {
        import digestion : hashOf2;
        return hashToIndex(hashOf2!(hasher)(key));
    }

}

@safe:

pure nothrow @nogc unittest
{
    import std.algorithm.comparison : equal;
    import digestx.fnv : FNV;

    immutable n = 600;

    alias K = uint;

    import std.meta : AliasSeq;
    foreach (V; AliasSeq!(void, string))
    {
        alias X = HashMapOrSet!(K, V, null, FNV!(64, true));

        static if (!X.hasValue)
        {
            X x;

            x.insert(11);
            x.insert(12);
            x.insert(13);

            import std.algorithm : count;
            auto xr = x.byElement;

            alias R = typeof(xr);
            import std.range : isInputRange;
            import std.traits : ReturnType;
            static assert(is(typeof(R.init) == R));
            static assert(is(ReturnType!((R xr) => xr.empty) == bool));
            auto f = xr.front;
            static assert(is(typeof((R xr) => xr.front)));
            static assert(!is(ReturnType!((R xr) => xr.front) == void));
            static assert(is(typeof((R xr) => xr.popFront)));

            static assert(isInputRange!(typeof(xr)));

            assert(x.byElement.count == 3);

            X y;
            foreach (const ref e; x.byElement)
            {
                y.insert(e);
            }

            assert(y.byElement.count == 3);
            assert(x == y);

            const z = X();
            assert(z.byElement.count == 0);

            immutable w = X();
            assert(w.byElement.count == 0);
        }

        import container_traits : mustAddGCRange;
        static if (X.hasValue &&
                   is(V == string))
        {
            static assert(mustAddGCRange!V);
            static assert(mustAddGCRange!(V[1]));
            static assert(mustAddGCRange!(X.T));
            static assert(mustAddGCRange!(X.SmallBin));
            static assert(!mustAddGCRange!(X.LargeBin));
        }
        else
        {
            static assert(!mustAddGCRange!(X.T));
            static assert(!mustAddGCRange!(X.SmallBin), "Fails for X being " ~ X.SmallBin.stringof);
        }

        auto x1 = X();            // start empty

        // all bins start small
        assert(x1.binCounts.largeCount == 0);

        // fill x1

        foreach (immutable key; 0 .. n)
        {
            static if (X.hasValue)
            {
                const value = V.init;
                const element = X.ElementType(key, value);
            }
            else
            {
                const element = key;
            }

            assert(key !in x1);

            assert(x1.length == key);
            assert(x1.insert(element) == InsertionStatus.added);

            static if (X.hasValue)
            {
                const e2 = X.ElementType(key, "a");
                assert(x1.insert(e2) == InsertionStatus.modified);
                assert(x1.contains(key));
                assert(x1.get(key, null) == "a");
                x1.remove(key);
                x1[key] = value;
            }

            assert(x1.length == key + 1);

            assert(key in x1);
            static if (X.hasValue)
            {
                auto elementFound = key in x1;
                assert(elementFound);
                assert(*elementFound != "_");
            }

            assert(x1.insert(element) == InsertionStatus.unmodified);
            static if (X.hasValue)
            {
                assert(x1.insert(key, value) == InsertionStatus.unmodified);
            }
            assert(x1.length == key + 1);

            assert(key in x1);
        }

        static if (X.hasValue)
        {
            import basic_array : Array = BasicArray;
            Array!(X.ElementType) a1;

            foreach (const ref key; x1.byKey)
            {
                auto keyPtr = key in x1;
                assert(keyPtr);
                a1 ~= X.ElementType(key, (*keyPtr));
            }

            assert(x1.length == a1.length);

            foreach (aElement; a1[])
            {
                auto keyPtr = aElement.key in x1;
                assert(keyPtr);
                assert((*keyPtr) is aElement.value);
            }
        }

        assert(x1.length == n);

        // duplicate x1

        auto x2 = x1.dup;

        // non-symmetric algorithm so both are needed
        assert(x2 == x1);
        assert(x1 == x2);

        static if (X.hasValue)
        {
            assert(equal(x1.byKey, x2.byKey));
            assert(equal(x1.byValue, x2.byValue));
            assert(equal(x1.byKeyValue, x2.byKeyValue));
            assert(equal(x1[], x2[]));
        }

        assert(x1.binCounts.largeCount ==
               x2.binCounts.largeCount);

        static assert(!__traits(compiles, { const _ = x1 < x2; })); // no ordering

        assert(x2.length == n);

        // empty x1

        foreach (immutable key; 0 .. n)
        {
            static if (X.hasValue)
            {
                const element = X.ElementType(key, V.init);
            }
            else
            {
                const element = key;
            }

            assert(x1.length == n - key);

            auto elementFound = key in x1;
            assert(elementFound);
            static if (X.hasValue)
            {
                assert(*elementFound is element.value);
            }

            assert(x1.remove(key));
            assert(x1.length == n - key - 1);

            static if (!X.hasValue)
            {
                assert(!x1.contains(key));
            }
            assert(key !in x1);
            assert(!x1.remove(key));
            assert(x1.length == n - key - 1);
        }

        assert(x1.binCounts.largeCount == 0);

        assert(x1.length == 0);

        x1.clear();
        assert(x1.length == 0);

        // empty x2

        assert(x2.length == n); // should be not affected by emptying of x1

        foreach (immutable key; 0 .. n)
        {
            static if (X.hasValue)
            {
                const element = X.ElementType(key, V.init);
            }
            else
            {
                const element = key;
            }

            assert(x2.length == n - key);

            assert(key in x2);

            assert(x2.remove(key));
            assert(x2.length == n - key - 1);

            assert(key !in x2);
            assert(!x2.remove(key));
            assert(x2.length == n - key - 1);
        }

        assert(x2.binCounts.largeCount == 0);

        assert(x2.length == 0);

        x2.clear();
        assert(x2.length == 0);
    }
}

/// range checking
@trusted pure unittest
{
    import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError;
    import uncopyable_sample : V = SomeUncopyable;
    import digestx.fnv : FNV;

    immutable n = 11;

    alias K = uint;

    alias X = HashMapOrSet!(K, V, null, FNV!(64, true));

    auto s = X.withCapacity(n);

    void dummy(ref V value) {}

    assertThrown!RangeError(dummy(s[K.init]));

    foreach (immutable uint i; 0 .. n)
    {
        s[i] = V(i);
        assertNotThrown!RangeError(dummy(s[i]));
    }

    foreach (immutable uint i; 0 .. n)
    {
        s.remove(i);
        assertThrown!RangeError(dummy(s[i]));
    }

    s[K.init] = V.init;
    auto vp = K.init in s;
    static assert(is(typeof(vp) == V*));
    assert((*vp) == V.init);

    s.remove(K.init);
    assert(K.init !in s);

    X t;
    t.reserveExtra(4096);
    assert(t.binCount == 8192);
}

/// class as value
@trusted pure unittest
{
    import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError;
    import digestx.fnv : FNV;

    immutable n = 11;

    alias K = uint;
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = HashMapOrSet!(K, V, null, FNV!(64, true));

    auto s = X.withCapacity(n);

    void dummy(ref V value) {}

    assertThrown!RangeError(dummy(s[K.init]));

    foreach (immutable uint i; 0 .. n)
    {
        s[i] = new V(i);
        assertNotThrown!RangeError(dummy(s[i]));
    }

    foreach (immutable uint i; 0 .. n)
    {
        s.remove(i);
        assertThrown!RangeError(dummy(s[i]));
    }

    s[K.init] = V.init;
    auto vp = K.init in s;
    static assert(is(typeof(vp) == V*));

    s.remove(K.init);
    assert(K.init !in s);

    X t;
    t.reserveExtra(4096);
    assert(t.binCount == 8192);
}

/// constness inference of ranges
pure nothrow unittest
{
    import digestx.fnv : FNV;

    alias K = uint;
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
    const x = X();

    foreach (e; x.byKey)
    {
        static assert(is(typeof(e) == const(X.KeyType)));
    }

    foreach (e; x.byValue)
    {
        static assert(is(typeof(e) == const(X.ValueType)));
    }

    foreach (e; x.byKeyValue)
    {
        static assert(is(typeof(e.key) == const(X.KeyType)));
        static assert(is(typeof(e.value) == const(X.ValueType)));
        static assert(is(typeof(e) == const(X.ElementType)));
    }
}

/// class mutability
pure nothrow unittest
{
    import digestx.fnv : FNV;

    alias K = uint;
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
    auto x = X();

    foreach (e; x.byValue)
    {
        // TODO static assert(is(typeof(e) == X.ValueType));
    }

    foreach (e; x.byKeyValue)
    {
        static assert(is(typeof(e.key) == const(X.KeyType)));
        // TODO static assert(is(typeof(e.value) == X.ValueType));
    }
}
