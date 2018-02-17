module open_hashmap_or_hashset;

import container_traits;
import pure_mallocator : PureMallocator;

@safe:

/** Hash set (or map) storing (key) elements of type `K` and values of type `V`.
 *
 * Uses open-addressing with quadratic probing using triangular numbers.
 *
 * Params:
 *      K = key type.
 *      V = value type.
 *      Allocator = memory allocator for bin array
 *      hasher = hash function or std.digest Hash.
 *
 * See also: https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/
 *
 * TODO extend opBinaryRight to return a reference to a free slot when assigned to sets value in slot and does _count += 1;
 *
 * TODO support HashSet-in operator: assert(*("a" in s) == "a");
 *
 * TODO add extractElement that moves it out similar to
 * http://en.cppreference.com/w/cpp/container/unordered_set/extract
 *
 * TODO growWithExtraCapacity(): if allocator has realloc we can do rehashing in-place similar to
 * reordering in in-place radix (integer_sorting.d), otherwise rehash into new
 * copy of bins and free old bins when done. If bin element count is >
 * 1 this is more complicated since each bin contains a set of elements to
 * swap out and must be put in a queue.
 *
 * TODO benchmark against https://github.com/greg7mdp/sparsepp
 *
 * TODO add merge or union algorithm here or into container_algorithm.d. See
 * also: http://en.cppreference.com/w/cpp/container/unordered_set/merge. this
 * algorithm moves elements from source if they are not already in `this`
 */
struct HashMapOrSet(K, V = void,
                    alias hasher = hashOf,
                    alias Allocator = PureMallocator.instance)
    if (isNullableType!K
        //isHashable!K
        )
{
    import std.conv : emplace;
    import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor, isCopyable, isMutable, hasIndirections;
    import std.meta : Unqual;
    import std.algorithm.comparison : max;
    import std.algorithm.mutation : move, moveEmplace;
    import emplace_all : moveEmplaceAllNoReset;
    import digestion : hashOf2;
    import probing : triangularProbeFromIndex;
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
        /** Map insertion status.
         */
        enum InsertionStatus
        {
            added,                      // element was added
            modified,                   // value of element was changed (map only). TODO only for Map-case
            unmodified                  // element was left unchanged
        }

        /// Constant element reference with both constant key and value.
        struct T
        {
            K key;
            V value;
        }

        /// Mutable element reference with constant key and mutable value.
        struct CT
        {
            const K key;
            V value;
        }

        /// Get key part of element.
        pragma(inline, true)
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            return element.key;
        }

        /// Get value part of element.
        pragma(inline, true)
        static auto ref inout(V) valueOf()(auto ref return inout(T) element)
        {
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


        alias T = K;

        /// Get key part of element.
        pragma(inline, true)
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            return element;
        }

        enum keyEqualPred = "a is b";

        enum nullKeyElement = defaultNullKeyConstantOf!K;
    }

    alias ElementType = T;

    pragma(inline)              // LDC can, DMD cannot inline
    static typeof(this) withCapacity_(size_t capacity) // template-lazy
        nothrow
    {
        import std.math : nextPow2;
        immutable powerOf2Capacity = capacity >= 3 ? nextPow2(capacity - 1) : capacity;

        import std.experimental.allocator : makeArray;
        typeof(return) result;
        result._bins = Allocator.makeArray!T(powerOf2Capacity, nullKeyElement);
        result._count = 0;
        return result;
    }

    private pragma(inline, true)
    void[] allocateBins(size_t byteCount) const pure nothrow @nogc @system
    {
        return Allocator.instance.allocate(T.sizeof*binCount);
    }

    /** Make with room for storing at least `capacity` number of elements.
     *
     * TODO remove these hacks when a solution is proposed at
     * https://forum.dlang.org/post/nyngzsaeqxzzuumivtze@forum.dlang.org
     *
     * See also:
     * https://forum.dlang.org/post/nyngzsaeqxzzuumivtze@forum.dlang.org
     */
    static if (is(typeof(Allocator) == immutable(PureMallocator)))
    {
        private pragma(inline, true)
        public static typeof(this) withCapacity(size_t capacity) // template-lazy
            @trusted
        {
            import hacks : assumePureNogc;
            return assumePureNogc(&withCapacity_)(capacity);
        }
    }
    else
    {
        public alias withCapacity = withCapacity_;
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

    /// No copying.
    @disable this(this);

    static if (isCopyable!T)
    {
        /// Returns: a shallow duplicate of `this`.
        typeof(this) dup()() const // template-lazy
            @trusted
        {
            T[] binsCopy = cast(T[])allocateBins(_bins.length);
            foreach (immutable elementIndex, ref element; _bins)
            {
                /** TODO functionize to `emplaceAll` in emplace_all.d. See also:
                 * http://forum.dlang.org/post/xxigbqqflzwfgycrclyq@forum.dlang.org
                 */
                if (keyOf(element).isNull)
                {
                    binsCopy[elementIndex] = T.init;
                }
                else
                {
                    static if (hasElaborateDestructor!T)
                    {
                        import std.conv : emplace;
                        emplace(&binsCopy[elementIndex], element);
                    }
                    else
                    {
                        binsCopy[elementIndex] = element;
                    }
                }
            }
            import std.algorithm.comparison : equal;
            assert(equal!((a, b) => a is b)(binsCopy, _bins));
            return typeof(return)(binsCopy, _count);
        }
    }

    /// Equality.
    bool opEquals()(const scope auto ref typeof(this) rhs) const
    {
        if (_count != rhs._count) { return false; }

        foreach (immutable ix; 0 .. _bins.length)
        {
            if (!keyOf(_bins[ix]).isNull)
            {
                static if (hasValue)
                {
                    auto elementFound = _bins[ix].key in rhs;
                    if (!elementFound)
                    {
                        return false;
                    }
                    if ((*elementFound) !is _bins[ix].value)
                    {
                        return false;
                    }
                }
                else
                {
                    if (!rhs.contains(_bins[ix])) { return false; }
                }
            }
        }

        return true;
    }

    /// Empty.
    void clear()()              // template-lazy
        @trusted pure
    {
        Allocator.instance.deallocate(_bins);
        _bins = typeof(_bins).init;
        _count = 0;
    }

    version(LDC) { pragma(inline, true): } // needed for LDC to inline this, DMD cannot
    pragma(inline, true):                  // LDC must have this

    /** Check if `element` is stored.
        Returns: `true` if element is present, `false` otherwise.
    */
    pragma(inline, true)
    bool contains()(const scope K key) const // template-lazy, auto ref here makes things slow
    {
        assert(!key.isNull);
        immutable startIndex = hashToIndex(hashOf2!(hasher)(key));
        immutable hitIndex = _bins[].triangularProbeFromIndex!(_ => keyOf(_) is key)(startIndex);
        return hitIndex != _bins.length;
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    pragma(inline, true)
    InsertionStatus insert(T element)
    {
        assert(!keyOf(element).isNull); // TODO needed?
        reserveExtra(1);
        return insertWithoutGrowth(move(element));
    }

    /** Insert `elements`, all being either a key-value (map-case) or a just a key (set-case).
     */
    void insertN(R)(R elements) @trusted
        if (isIterable!R &&
            isCopyable!T)       // TODO support uncopyable T?
    {
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

    /** Reserve rom for `extraCapacity` number of extra buckets. */
    void reserveExtra()(size_t extraCapacity)
    {
        if ((_count + extraCapacity) * 2 > _bins.length)
        {
            growWithExtraCapacity(extraCapacity);
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
            newBinCount = _count + extraCapacity;
        }
        auto copy = withCapacity(newBinCount);

        // move elements to copy
        foreach (immutable ix; 0 .. _bins.length)
        {
            if (!keyOf(_bins[ix]).isNull)
            {
                copy.insertMoveWithoutGrowth(_bins[ix]);
            }
        }
        assert(copy._count == _count);

        move(copy._bins, _bins);

        assert(_bins.length);
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    pragma(inline, true)
    private InsertionStatus insertWithoutGrowth(T element)
    {
        assert(!keyOf(element).isNull);

        immutable startIndex = hashToIndex(hashOf2!(hasher)(keyOf(element)));
        immutable hitIndex = _bins[].triangularProbeFromIndex!(_ => (keyOf(_) is keyOf(element) ||
                                                                     keyOf(_).isNull))(startIndex);
        assert(hitIndex != _bins.length, "no free slot");

        if (keyOf(_bins[hitIndex]).isNull) // key missing
        {
            move(element,
                 _bins[hitIndex]);
            _count += 1;
            return InsertionStatus.added;
        }
        static if (hasValue)
        {
            if (valueOf(element) !is
                valueOf(_bins[hitIndex])) // only value changed
            {
                move(valueOf(element),
                     valueOf(_bins[hitIndex]));
                return InsertionStatus.modified;
            }
        }
        return InsertionStatus.unmodified;
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    pragma(inline, true)
    private InsertionStatus insertMoveWithoutGrowth(ref T element)
    {
        return insertWithoutGrowth(move(element));
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

    /** L-value element reference (and in turn range iterator).
     */
    static private struct LvalueElementRef(SomeHashMapOrSet)
    {
        SomeHashMapOrSet* table;
        size_t iterationIndex;  // index to bin inside `table`
        size_t iterationCounter; // counter over number of elements popped

        pragma(inline, true):

        /// Check if empty.
        @property bool empty() const @safe pure nothrow @nogc
        {
            return iterationIndex == table.binCount;
        }

        /// Get number of element left to pop.
        @property size_t length() const @safe pure nothrow @nogc
        {
            return table.length - iterationCounter;
        }

        pragma(inline)
        void popFront()
        {
            assert(!empty);
            iterationIndex += 1;
            findNextNonEmptyBin();
            iterationCounter += 1;
        }

        @property typeof(this) save() // ForwardRange
        {
            return this;
        }

        private void findNextNonEmptyBin()
        {
            while (iterationIndex != (*table).binCount &&
                   keyOf((*table)._bins[iterationIndex]).isNull)
            {
                iterationIndex += 1;
            }
        }
    }

    /** R-value element reference (and in turn range iterator).
     */
    static private struct RvalueElementRef(SomeHashMapOrSet)
    {
        SomeHashMapOrSet table; // owned
        size_t iterationIndex;  // index to bin inside table
        size_t iterationCounter; // counter over number of elements popped

        pragma(inline, true):

        /// Check if empty.
        @property bool empty() const @safe pure nothrow @nogc
        {
            return iterationIndex == table.binCount;
        }

        /// Get number of element left to pop.
        @property size_t length() const @safe pure nothrow @nogc
        {
            return table.length - iterationCounter;
        }

        pragma(inline)
        void popFront()
        {
            assert(!empty);
            iterationIndex += 1;
            findNextNonEmptyBin();
            iterationCounter += 1;
        }

        private void findNextNonEmptyBin()
        {
            while (iterationIndex != table.binCount &&
                   keyOf(table._bins[iterationIndex]).isNull)
            {
                iterationIndex += 1;
            }
        }
    }

    static if (!hasValue)       // HashSet
    {
        pragma(inline, true)
        bool opBinaryRight(string op)(const scope K key) const
            if (op == "in")
        {
            return contains(key);
        }

        /// Range over elements of l-value instance of this.
        static private struct ByLvalueElement(SomeHashMapOrSet)
        {
        pragma(inline, true):
            static if (is(T == class))
            {
                /// Get reference to front element (key and value).
                @property scope auto front()() return
                {
                    /* cast away const from `SomeHashMapOrSet` for classes
                     * because class elements are currently hashed and compared
                     * compared using their identity (pointer value) `is`
                     */
                    return cast(T)table.binElementsAt(ix)[elementOffset];
                }
            }
            else
            {
                /// Get reference to front element (key and value).
                @property scope auto front()()
                {
                    return table._bins[iterationIndex];
                }
            }
            public LvalueElementRef!SomeHashMapOrSet _elementRef;
            alias _elementRef this;
        }

        /// Range over elements of r-value instance of this.
        static private struct ByRvalueElement(SomeHashMapOrSet)
        {
        pragma(inline, true):
            static if (is(T == class))
            {
                /// Get reference to front element (key and value).
                @property scope auto front()() return
                {
                    /* cast away const from `SomeHashMapOrSet` for classes
                     * because class elements are currently hashed and compared
                     * compared using their identity (pointer value) `is`
                     */
                    return cast(T)table.binElementsAt(iterationIndex)[elementOffset];
                }
            }
            else
            {
                /// Get reference to front element (key and value).
                @property scope auto front()()
                {
                    return table._bins[iterationIndex];
                }
            }
            public RvalueElementRef!SomeHashMapOrSet _elementRef;
            alias _elementRef this;
        }

        /// ditto
        version(none)           // cannot be combined
        {
        pragma(inline, true)
        scope auto opSlice()() inout return // template-lazy
        {
            return byElement();
        }
        }
    }

    static if (hasValue)        // HashMap
    {
        scope inout(V)* opBinaryRight(string op)(const scope K key) inout return // auto ref here makes things slow
            if (op == "in")
        {
            immutable startIndex = hashToIndex(hashOf2!(hasher)(key));
            immutable hitIndex = _bins[].triangularProbeFromIndex!(_ => keyOf(_) is key)(startIndex);
            if (hitIndex != _bins.length) // if hit
            {
                return cast(typeof(return))&_bins[hitIndex].value;
            }
            else                    // miss
            {
                return null;
            }
        }

        static private struct ByKey(SomeHashMapOrSet)
        {
            pragma(inline, true):
            /// Get reference to key of front element.
            @property scope const auto ref front()() return // key access must be const
            {
                return table._bins[iterationIndex].key;
            }
            public LvalueElementRef!SomeHashMapOrSet _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys of `this` in undefined order.
        @property scope auto byKey()() inout return // template-lazy property
        {
            alias This = ConstThis;
            auto result = ByKey!This((LvalueElementRef!This(cast(This*)&this)));
            result.findNextNonEmptyBin();
            return result;
        }

        static private struct ByValue(SomeHashMapOrSet)
        {
            pragma(inline, true):
            /// Get reference to value of front element.
            @property scope auto ref front()() return @trusted // template-lazy property
            {
                return *(cast(ValueType*)&table._bins[iterationIndex].value);
            }
            public LvalueElementRef!SomeHashMapOrSet _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the values of `this` in undefined order.
        @property scope auto byValue()() inout return // template-lazy property
        {
            alias This = ConstThis;
            auto result = ByValue!This((LvalueElementRef!This(cast(This*)&this)));
            result.findNextNonEmptyBin();
            return result;
        }

        static private struct ByKeyValue(SomeHashMapOrSet)
        {
            pragma(inline, true):
            /// Get reference to front element (key and value).
            @property scope auto ref front()() return @trusted
            {
                static if (isMutable!(SomeHashMapOrSet))
                {
                    alias E = CT;
                }
                else
                {
                    alias E = const(T);
                }
                return *(cast(E*)&table._bins[iterationIndex]);
            }
            public LvalueElementRef!SomeHashMapOrSet _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys and values of `this`.
        @property scope auto byKeyValue()() return // template-lazy property
        {
            alias This = MutableThis;
            auto result = ByKeyValue!This((LvalueElementRef!This(cast(This*)&this)));
            result.findNextNonEmptyBin();
            return result;
        }
        /// ditto
        @property scope auto byKeyValue()() const return // template-lazy property
        {
            alias This = ConstThis;
            auto result = ByKeyValue!This((LvalueElementRef!This(cast(This*)&this)));
            result.findNextNonEmptyBin();
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
        scope ref inout(V) opIndex()(const scope K key) inout return // auto ref here makes things slow
        {
            immutable startIndex = hashToIndex(hashOf2!(hasher)(key));
            immutable hitIndex = _bins[].triangularProbeFromIndex!(_ => keyOf(_) is key)(startIndex);
            if (hitIndex != _bins.length)
            {
                return _bins[hitIndex].value;
            }
            else
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
        auto ref V get()(const scope K key,
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

	/** Supports $(B aa[key] = value;) syntax.
	 */
        pragma(inline, true)
        void opIndexAssign()(V value, K key) // template-lazy
	{
            insert(T(move(key),
                     move(value)));
            // TODO return reference to value
	}
    }

    /** Remove `element`.
        Returns: `true` if element was removed, `false` otherwise.
    */
    bool remove()(const scope K key) // template-lazy
    {
        immutable startIndex = hashToIndex(hashOf2!(hasher)(key));
        immutable hitIndex = _bins[].triangularProbeFromIndex!(_ => keyOf(_) is key)(startIndex);
        if (hitIndex != _bins.length) // if hit
        {
            keyOf(_bins[hitIndex]).nullify();
            static if (hasValue &&
                       hasElaborateDestructor!V)
            {
                valueOf(_bins[hitIndex]) = V.init;
                // TODO instead do only .destroy(valueOf(_bins[hitIndex]));
            }
            _count -= 1;
            return true;
        }
        return false;
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

private:
    T[] _bins;            // bin elements
    size_t _count;        // total number of non-null elements stored in `_bins`

    /** Returns: bin index of `hash`. */
    pragma(inline, true)
    size_t hashToIndex(const scope hash_t hash) const
    {
        return hash & powerOf2Mask;
    }

    /** Returns: current index mask from bin count. */
    pragma(inline, true)
    private size_t powerOf2Mask() const
    {
        immutable typeof(return) mask = _bins.length - 1;
        assert((~mask ^ mask) == typeof(mask).max); // isPowerOf2(_bins.length)
        return mask;
    }
}

/** Hash set storing keys of type `K`.
 */
alias OpenHashSet(K,
                  alias hasher = hashOf,
                  alias Allocator = PureMallocator.instance) = HashMapOrSet!(K, void, hasher, Allocator);

/** Hash map storing keys of type `K` and values of type `V`.
 */
alias OpenHashMap(K,
                  V,
                  alias hasher = hashOf,
                  alias Allocator = PureMallocator.instance) = HashMapOrSet!(K, V, hasher, Allocator);

import std.traits : isInstanceOf;

/** Reset (remove) all elements in `x` matching `predicate`.
*/
void resetAllMatching(alias predicate, SomeHashMapOrSet)(auto ref SomeHashMapOrSet x)
    if (isInstanceOf!(HashMapOrSet,
                      SomeHashMapOrSet))
{
    size_t count = 0;
    alias E = typeof(SomeHashMapOrSet._bins.init[0]);
    foreach (immutable i; 0 .. x._bins.length)
    {
        import std.functional : unaryFun;
        if (!x._bins[i].isNull &&
            unaryFun!predicate(x._bins[i]))
        {
            count += 1;
            x._bins[i].nullify();
        }
    }
    x._count -= count;
}

/** Returns: `x` eagerly filtered on `predicate`.
    TODO move to container_algorithm.d.
*/
SomeHashMapOrSet filtered(alias predicate, SomeHashMapOrSet)(SomeHashMapOrSet x)
    if (isInstanceOf!(HashMapOrSet,
                      SomeHashMapOrSet))
{
    import std.functional : not;
    x.resetAllMatching!(not!predicate);
    import std.algorithm.mutation : move;
    return move(x);
}

/** Returns: `x` eagerly intersected with `y`.
    TODO move to container_algorithm.d.
 */
auto intersectedWith(C1, C2)(C1 x, auto ref C2 y)
    if (isInstanceOf!(HashMapOrSet, C1) &&
        isInstanceOf!(HashMapOrSet, C2))
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
            return move(x).filtered!(_ => y.contains(_));
        }
        else
        {
            return move(y).filtered!(_ => x.contains(_));
        }
    }
}

/// r-value and l-value intersection
@safe pure nothrow @nogc unittest
{
    alias K = Nullable!(uint, uint.max);
    alias X = HashMapOrSet!(K, void, FNV!(64, true));

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
}

/// r-value and r-value intersection
@safe pure nothrow @nogc unittest
{
    alias K = Nullable!(uint, uint.max);
    alias X = HashMapOrSet!(K, void, FNV!(64, true));

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
    if (isInstanceOf!(HashMapOrSet, C1) &&
        isInstanceOf!(HashMapOrSet, C2))
{
    return x.resetAllMatching!(_ => !y.contains(_));
}

/// r-value and l-value intersection
@safe pure nothrow @nogc unittest
{
    alias K = Nullable!(uint, uint.max);
    alias X = HashMapOrSet!(K, void, FNV!(64, true));

    auto x = X.withElements([K(12), K(13)].s);
    auto y = X.withElements([K(10), K(12), K(13), K(15)].s);
    y.intersectWith(x);
    assert(y.length == 2);
    assert(y.contains(K(12)));
    assert(y.contains(K(13)));
}

/** Returns forward range that iterates through the elements of `c` in undefined
 * order.
 */
auto byElement(SomeHashMapOrSet)(auto ref inout(SomeHashMapOrSet) c)
    @trusted
    if (isInstanceOf!(HashMapOrSet,
                      SomeHashMapOrSet))
{
    alias C = const(SomeHashMapOrSet);
    static if (__traits(isRef, c))
    {
        auto result = C.ByLvalueElement!C((C.LvalueElementRef!C(cast(C*)&c)));
        result.findNextNonEmptyBin();
        return result;
    }
    else
    {
        import std.algorithm.mutation : move;
        auto result = C.ByRvalueElement!C((C.RvalueElementRef!C(move(*(cast(SomeHashMapOrSet*)&c))))); // reinterpret
        result.findNextNonEmptyBin();
        return move(result);
    }
}
alias range = byElement;        // EMSI-container naming

/// make range from l-value and r-value. element access is always const
pure nothrow @nogc unittest
{
    alias K = Nullable!(uint, uint.max);
    alias X = HashMapOrSet!(K, void, FNV!(64, true));

    immutable a = [K(11), K(22), K(33)].s;

    // mutable
    auto x = X.withElements(a);
    assert(x.length == 3);
    assert(x.byElement.count == x.length);
    foreach (e; x.byElement)    // from l-value
    {
        assert(x.contains(e));
        static assert(is(typeof(e) == const(K))); // always const access
    }

    // const
    const y = X.withElements(a);
    foreach (e; y.byElement)    // from l-value
    {
        assert(y.contains(e));
        static assert(is(typeof(e) == const(K)));
    }

    foreach (e; X.withElements([K(11)].s).byElement) // from r-value
    {
        assert(e == K(11));
        static assert(is(typeof(e) == const(K))); // always const access
    }
}

/// test various things
pure nothrow @nogc unittest
{
    immutable uint n = 600;

    alias K = Nullable!(uint, uint.max);

    import std.meta : AliasSeq;
    foreach (V; AliasSeq!(void, string))
    {
        alias X = HashMapOrSet!(K, V, FNV!(64, true));

        static if (!X.hasValue)
        {
            auto x = X.withElements([K(11), K(12), K(13)].s);

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
                assert(x.contains(e));
                assert(!y.contains(e));
                y.insert(e);
                assert(y.contains(e));
            }

            assert(y.byElement.count == 3);
            assert(x == y);

            const z = X();
            assert(z.byElement.count == 0);

            immutable w = X();
            assert(w.byElement.count == 0);

            {
                auto xc = X.withElements([K(11), K(12), K(13)].s);
                assert(xc.length == 3);
                assert(xc.contains(K(11)));

                // TODO http://forum.dlang.org/post/kvwrktmameivubnaifdx@forum.dlang.org
                xc.resetAllMatching!(_ => _ == K(11));

                assert(xc.length == 2);
                assert(!xc.contains(K(11)));

                xc.resetAllMatching!(_ => _ == 12);
                assert(!xc.contains(K(12)));
                assert(xc.length == 1);

                xc.resetAllMatching!(_ => _ == 13);
                assert(!xc.contains(K(13)));
                assert(xc.length == 0);

                // this is ok
                foreach (e; xc.byElement) {}

            }

            {
                auto k = X.withElements([K(11), K(12)].s).filtered!(_ => _ != K(11)).byElement;
                static assert(isInputRange!(typeof(k)));
                assert(k.front == 12);
                k.popFront();
                assert(k.empty);
            }

            {
                X q;
                auto qv = [K(11U), K(12U), K(13U), K(14U)].s;
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
        static if (X.hasValue &&
                   is(V == string))
        {
            static assert(mustAddGCRange!V);
            static assert(mustAddGCRange!(V[1]));
            static assert(mustAddGCRange!(X.T));
        }
        else
        {
            static assert(!mustAddGCRange!(X.T));
        }

        auto x1 = X();            // start empty

        // fill x1

        foreach (immutable key_; 0 .. n)
        {
            const key = K(key_);

            static if (X.hasValue)
            {
                const value = V.init;
                const element = X.ElementType(key, value);
            }
            else
            {
                // no assignment because Nullable.opAssign may leave rhs in null state
                alias element = key;
            }

            assert(key !in x1);

            assert(x1.length == key);
            assert(x1.insert(element) == X.InsertionStatus.added);
            assert(x1.length == key + 1);

            static if (X.hasValue)
            {
                const e2 = X.ElementType(key, "a");
                assert(x1.insert(e2) == X.InsertionStatus.modified);
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

            assert(x1.insert(element) == X.InsertionStatus.unmodified);
            static if (X.hasValue)
            {
                assert(x1.insert(key, value) == X.InsertionStatus.unmodified);
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

        static assert(!__traits(compiles, { const _ = x1 < x2; })); // no ordering

        assert(x2.length == n);

        // empty x1

        foreach (immutable key_; 0 .. n)
        {
            const key = K(key_);

            static if (X.hasValue)
            {
                const element = X.ElementType(key, V.init);
            }
            else
            {
                alias element = key;
            }

            assert(x1.length == n - key);

            const elementFound = key in x1;
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

        assert(x1.length == 0);

        x1.clear();
        assert(x1.length == 0);

        // empty x2

        assert(x2.length == n); // should be not affected by emptying of x1

        foreach (immutable key_; 0 .. n)
        {
            const key = K(key_);

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

        assert(x2.length == 0);

        x2.clear();
        assert(x2.length == 0);
    }
}

/// range checking
@trusted pure unittest
{
    immutable n = 11;

    alias K = Nullable!(uint, uint.max);
    alias V = uint;

    alias X = HashMapOrSet!(K, V, FNV!(64, true));

    auto s = X.withCapacity(n);

    void dummy(ref V value) {}

    assertThrown!RangeError(dummy(s[K(0)]));

    foreach (immutable uint i; 0 .. n)
    {
        const k = K(i);
        s[k] = V(i);
        assertNotThrown!RangeError(dummy(s[k]));
    }

    foreach (immutable uint i; 0 .. n)
    {
        const k = K(i);
        s.remove(k);
        assertThrown!RangeError(dummy(s[k]));
    }

    s[K(0)] = V.init;
    auto vp = K(0) in s;
    static assert(is(typeof(vp) == V*));
    assert((*vp) == V.init);

    s.remove(K(0));
    assert(K(0) !in s);

    X t;
    t.reserveExtra(4096);

    t.clear();
}

/// class as value
@trusted pure unittest
{
    immutable n = 11;

    alias K = Nullable!(uint, uint.max);
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = HashMapOrSet!(K, V, FNV!(64, true));

    auto s = X.withCapacity(n);

    void dummy(ref V value) {}

    assertThrown!RangeError(dummy(s[K(0)]));

    foreach (immutable uint i; 0 .. n)
    {
        const k = K(i);
        s[k] = new V(i);
        assertNotThrown!RangeError(dummy(s[k]));
    }

    // test range
    auto sr = s.byKeyValue;
    assert(sr.length == n);
    foreach (immutable uint i; 0 .. n)
    {
        sr.popFront();
        assert(sr.length == n - i - 1);
    }

    foreach (immutable uint i; 0 .. n)
    {
        const k = K(i);
        s.remove(k);
        assertThrown!RangeError(dummy(s[k]));
    }

    s[K(0)] = V.init;
    auto vp = K(0) in s;
    static assert(is(typeof(vp) == V*));

    s.remove(K(0));
    assert(K(0) !in s);

    X t;
    t.reserveExtra(4096);
}

/// constness inference of ranges
pure nothrow unittest
{
    alias K = Nullable!(uint, uint.max);
    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = HashMapOrSet!(K, V, FNV!(64, true));
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
    struct S
    {
        uint value;
    }
    alias K = Nullable!(S, S(uint.max));

    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = HashMapOrSet!(K, V, FNV!(64, true));
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

        assert(e.value.data == 43);

        // value mutation side effects
        e.value.data += 1;
        assert(e.value.data == 44);
        e.value.data -= 1;
        assert(e.value.data == 43);
    }
}

version(unittest)
{
    import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError;
    import std.algorithm : count;
    import std.algorithm.comparison : equal;
    import std.typecons : Nullable;
    import digestx.fnv : FNV;
    import array_help : s;
    import dbgio;
}
