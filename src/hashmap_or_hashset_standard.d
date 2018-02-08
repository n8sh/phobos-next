module hashmap_or_hashset_standard;

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
 * TODO when allocating _bins use nullKeyConstant for assignment after
 * allocating and in allocator when that is used.
 *
 * TODO try quadratic probing using triangular numbers:
 * http://stackoverflow.com/questions/2348187/moving-from-linear-probing-to-quadratic-probing-hash-collisons/2349774#2349774
 *
 * TODO in non-sso optimized store add flag similar to Nullable that reserves a
 * specific value for key that indicates that slot is unused. Use this when
 * storing indexes in knet.storage
 *
 * TODO add extractElement that moves it out similar to
 * http://en.cppreference.com/w/cpp/container/unordered_set/extract
 *
 * TODO add merge or union algorithm here or into container_algorithm.d. See
 * also: http://en.cppreference.com/w/cpp/container/unordered_set/merge. this
 * algorithm moves elements from source if they are not already in `this`
 *
 * TODO add flag for use of growth factor smaller than powers of two. use prime_modulo.d
 *
 * TODO use core.bitop : bsr, bsl to find first empty element in bin. if as fast
 * as current find use it to optimize remove()
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
                    K nullKeyConstant = K.init)
    // if (isHashable!K)
{
    import std.conv : emplace;
    import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor, isCopyable, isMutable, hasIndirections;
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

        /// Get reference to key part of `element`.
        pragma(inline, true)
        static ref inout(K) keyRefOf()(ref return inout(T) element) // template-lazy
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
    }
    else                        // HashSet
    {
        alias T = K;

        /// Get key part of element.
        pragma(inline, true)
        static auto ref inout(K) keyOf()(auto ref return inout(T) element)
        {
            return element;
        }

        /// Get reference to key part of `element`.
        pragma(inline, true)
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
        return typeof(return)(Bins.withCapacity(capacity), 0);
    }

    pragma(inline)              // LDC can, DMD cannot inline
    private static typeof(this) withBinCount()(size_t binCount) // template-lazy
    {
        typeof(return) that;    // TODO return direct call to store constructor
        that._bins = Bins.withLength(binCount);
        that._length = 0;
        return that;
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

    /// Duplicate.
    static if (isCopyable!T)
    {
        typeof(this) dup()() const // template-lazy
        {
            return typeof(return)(_bins.dup, _length);
        }
    }

    /// Equality.
    bool opEquals()(in auto ref typeof(this) rhs) const @trusted
    {
        if (_length != rhs._length) { return false; }

        foreach (immutable ix; 0 .. _bins.length)
        {
            if (keyOf(_bins[ix]) !is nullKeyConstant)
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
    {
        _bins.clear();
    }

    version(LDC) { pragma(inline, true): } // needed for LDC to inline this, DMD cannot
    pragma(inline, true):                  // LDC must have this

    /** Check if `element` is stored.
        Returns: `true` if element was already present, `false` otherwise.
     */
    bool contains()(in K key) const // template-lazy. TODO make `auto ref K` work
    {
        if (empty)              // TODO can this check be avoided?
        {
            return false; // prevent `RangeError` in `_bins` when empty
        }
        immutable ix = tryFindKeyIx(key);
        return ix != _bins.length && keyOf(_bins[key]) is key;
    }
    /// ditto
    bool contains()(in ref K key) const // template-lazy
    {
        if (empty)              // TODO can this check be avoided?
        {
            return false; // prevent `RangeError` in `_bins` when empty
        }
        immutable ix = tryFindKeyIx(key);
        return ix != _bins.length && keyOf(_bins[key]) is key;
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    pragma(inline, true)
    InsertionStatus insert(T element)
    {
        reserveExtra(1);
        return insertWithoutGrowth(move(element));
    }

    /** Insert `elements`, all being either a key-value (map-case) or a just a key (set-case).
     */
    void insertN(R)(R elements) @trusted
        if (isIterable!R &&
            isCopyable!T)
    {
        import std.range : hasLength;
        static if (hasLength!R)
        {
            // reserveExtra(elements.length); // TODO this fails
        }
        foreach (element; elements)
        {
            // TODO use `insertWithoutGrowth` when call to `reserveExtra` works
            static if (hasIndirections!T)
            {
                insert(element);
            }
            else
            {
                insert(*cast(Unqual!T*)&element);
            }
        }
    }

    /** Reserve rom for `extraCapacity` number of extra buckets. */
    void reserveExtra()(size_t extraCapacity)
    {
        if ((_length + extraCapacity) * 2 > _bins.length)
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
            newBinCount = _length + extraCapacity;
        }
        auto copy = withBinCount(newBinCount);

        // move elements to copy
        foreach (immutable ix; 0 .. _bins.length)
        {
            copy.insertMoveWithoutGrowth(_bins[ix]);
        }
        assert(copy._length == _length); // length should stay same

        move(copy._bins, _bins);

        assert(!_bins.empty);
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    pragma(inline, true)
    private InsertionStatus insertWithoutGrowth(T element)
    {
        immutable ix = keyToIx(keyOf(element));
        assert(ix != _bins.length); // not full
        immutable status = keyOf(_bins[ix]) is nullKeyConstant ? InsertionStatus.added : InsertionStatus.unmodified;
        _length += status == InsertionStatus.added ? 1 : 0;
        move(element, _bins[ix]);
        return status;
    }

    /** Insert `element`, being either a key-value (map-case) or a just a key (set-case).
     */
    pragma(inline, true)
    private InsertionStatus insertMoveWithoutGrowth(ref T element)
    {
        immutable ix = keyToIx(keyOf(element));
        assert(ix != _bins.length); // not full
        immutable status = keyOf(_bins[ix]) is nullKeyConstant ? InsertionStatus.added : InsertionStatus.unmodified;
        _length += status == InsertionStatus.added ? 1 : 0;
        move(element, _bins[ix]);
        return status;
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
    static private struct LvalueElementRef(HashMapOrSetType)
    {
        HashMapOrSetType* table;
        size_t ix;           // index to bin inside table
        size_t elementCounter;  // counter over number of elements popped

        pragma(inline, true):

        /// Check if empty.
        @property bool empty() const @safe pure nothrow @nogc
        {
            return ix == table.binCount;
        }

        /// Get number of element left to pop.
        @property size_t length() const @safe pure nothrow @nogc
        {
            return table.length - elementCounter;
        }

        void initFirstNonEmptyBin()
        {
            assert(0, "set ix to first non-empty key or element");
        }

        pragma(inline)
        void popFront()
        {
            assert(!empty);
            assert(0, "set ix to next non-empty key or element");
        }

        @property typeof(this) save() // ForwardRange
        {
            return this;
        }
    }

    /** R-value element reference (and in turn range iterator).
     */
    static private struct RvalueElementRef(HashMapOrSetType)
    {
        HashMapOrSetType table; // owned
        size_t ix;           // index to bin inside table
        size_t elementCounter;  // counter over number of elements popped

        pragma(inline, true):

        /// Check if empty.
        @property bool empty() const @safe pure nothrow @nogc
        {
            return ix == table.binCount;
        }

        /// Get number of element left to pop.
        @property size_t length() const @safe pure nothrow @nogc
        {
            return table.length - elementCounter;
        }

        void initFirstNonEmptyBin()
        {
            assert(0, "set ix to first non-empty key or element");
        }

        pragma(inline)
        void popFront()
        {
            assert(!empty);
            assert(0, "set ix to next non-empty key or element");
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

        /// Range over elements of l-value instance of this.
        static private struct ByLvalueElement(HashMapOrSetType)
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
                    return cast(ElementType)table.binElementsAt(ix)[elementOffset];
                }
            }
            else
            {
                /// Get reference to front element (key and value).
                @property scope auto front()()
                {
                    return table._bins[ix];
                }
            }
            public LvalueElementRef!HashMapOrSetType _elementRef;
            alias _elementRef this;
        }

        /// Range over elements of r-value instance of this.
        static private struct ByRvalueElement(HashMapOrSetType)
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
                    return cast(ElementType)table.binElementsAt(ix)[elementOffset];
                }
            }
            else
            {
                /// Get reference to front element (key and value).
                @property scope auto front()()
                {
                    return table._bins[ix];
                }
            }
            public RvalueElementRef!HashMapOrSetType _elementRef;
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
        scope inout(V)* opBinaryRight(string op)(in K key) inout @trusted return
            if (op == "in")
        {
            if (empty)
            {
                // prevent range error in `_bins` when `this` is empty
                return typeof(return).init;
            }
            immutable ix = keyToIx(key);
            if (keyOf(_bins[ix]) !is nullKeyConstant) // if hit
            {
                return cast(typeof(return))&_bins[ix].value;
            }
            else                    // miss
            {
                return null;
            }
        }

        static private struct ByKey(HashMapOrSetType)
        {
            pragma(inline, true):
            /// Get reference to key of front element.
            @property scope const auto ref front()() return // key access must be const
            {
                return table._bins[ix].key;
            }
            public LvalueElementRef!HashMapOrSetType _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys of `this`.
        @property scope auto byKey()() inout return // template-lazy property
        {
            alias This = ConstThis;
            auto result = ByKey!This((LvalueElementRef!This(cast(This*)&this)));
            result.initFirstNonEmptyBin();
            return result;
        }

        static private struct ByValue(HashMapOrSetType)
        {
            pragma(inline, true):
            /// Get reference to value of front element.
            @property scope auto ref front()() return @trusted // template-lazy property
            {
                return *(cast(ValueType*)&table._bins[ix].value);
            }
            public LvalueElementRef!HashMapOrSetType _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the values of `this`.
        @property scope auto byValue()() inout return // template-lazy property
        {
            alias This = ConstThis;
            auto result = ByValue!This((LvalueElementRef!This(cast(This*)&this)));
            result.initFirstNonEmptyBin();
            return result;
        }

        static private struct ByKeyValue(HashMapOrSetType)
        {
            pragma(inline, true):
            /// Get reference to front element (key and value).
            @property scope auto ref front()() return @trusted
            {
                static if (isMutable!(HashMapOrSetType))
                {
                    alias E = CT;
                }
                else
                {
                    alias E = const(T);
                }
                return *(cast(E*)&table._bins[ix]);
            }
            public LvalueElementRef!HashMapOrSetType _elementRef;
            alias _elementRef this;
        }

        /// Returns forward range that iterates through the keys and values of `this`.
        @property scope auto byKeyValue()() return // template-lazy property
        {
            alias This = MutableThis;
            auto result = ByKeyValue!This((LvalueElementRef!This(cast(This*)&this)));
            result.initFirstNonEmptyBin();
            return result;
        }
        /// ditto
        @property scope auto byKeyValue()() const return // template-lazy property
        {
            alias This = ConstThis;
            auto result = ByKeyValue!This((LvalueElementRef!This(cast(This*)&this)));
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
            immutable ix = keyToIx(key);
            if (keyOf(_bins[ix]) !is nullKeyConstant) // if hit
            {
                return _bins[ix].value;
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
            auto value = key in this;
            if (value !is null) // hit
            {
                return *value;
            }
            else                // miss
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
    bool remove()(in K key)     // template-lazy
        @trusted
    {
        assert(0, "remove at ix");
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

private:
    import basic_array : Array = BasicArray;
    alias Bins = Array!(T, Allocator);

    Bins _bins;                 // bin elements
    size_t _length;             // total number of elements stored

    /** Returns: bin index of `hash`. */
    pragma(inline, true)
    size_t hashToIndex(in hash_t hash) const
    {
        return hash & powerOf2Mask;
    }

    /** Returns: bin index of `key`. */
    pragma(inline, true)
    size_t keyToIx()(in auto ref K key) const
    {
        import digestion : hashOf2;
        return hashToIndex(hashOf2!(hasher)(key));
    }

    /** Returns: bin index of `key` or empty bin or `_bins.length` if full. */
    size_t tryFindKeyIx()(in auto ref K key) const
    {
        // TODO use among?

        size_t ix = keyToIx(key);

        if (isIxForKey(key, ix))
        {
            return ix;
        }

        // if not yet decided
        immutable size_t mask = powerOf2Mask;
        ix = (ix + 1) % mask;   // modulo power of two

        size_t inc = 1;
        while (!isIxForKey(key, ix) &&
               inc != _bins.length)
        {
            ix = (ix + inc) % mask;
            inc *= 2;
        }

        if (isIxForKey(key, ix))
        {
            return ix;          // slot
        }
        else
        {
            return _bins.length; // no slot, full
        }
    }

    private size_t isIxForKey()(in auto ref K key, in size_t ix) const
    {
        return (keyOf(_bins[ix]) is key ||           // hit slot
                keyOf(_bins[ix]) is nullKeyConstant); // free slot
    }

    /** Returns: current index mask from bin count. */
    pragma(inline, true)
    private size_t powerOf2Mask() const
    {
        immutable typeof(return) mask = _bins.length - 1;
        assert((~mask ^ mask) == size_t.max); // isPowerOf2(_bins.length)
        return mask;
    }
}

import std.traits : isInstanceOf;
import std.functional : unaryFun;

/** Remove all elements in `x` matching `predicate`.
    TODO move to container_algorithm.d.
*/
void resetAllMatching(alias predicate, HashMapOrSetType)(auto ref HashMapOrSetType x)
    @trusted
    if (isInstanceOf!(HashMapOrSet,
                      HashMapOrSetType))
{
    import basic_array : resetAllMatching;
    immutable count = x._bins.resetAllMatching!predicate();
    x._length -= count;
}

/** Returns: `x` eagerly filtered on `predicate`.
    TODO move to container_algorithm.d.
*/
HashMapOrSetType filtered(alias predicate, HashMapOrSetType)(HashMapOrSetType x)
    @trusted
    if (isInstanceOf!(HashMapOrSet,
                      HashMapOrSetType))
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
    alias K = uint;
    alias X = HashMapOrSet!(K, void, null, FNV!(64, true));
    import dbgio;

    auto x = X.withElements([12, 13].s);
    assert(x.length == 2);
    auto y = X.withElements([10, 12, 13, 15].s).intersectedWith(x);
    dln(y.length);
    assert(y.length == 2);
    assert(y.contains(12));
    assert(y.contains(13));
}

/// r-value and r-value intersection
@safe pure nothrow @nogc unittest
{
    alias K = uint;
    alias X = HashMapOrSet!(K, void, null, FNV!(64, true));

    auto y = X.withElements([10, 12, 13, 15].s).intersectedWith(X.withElements([12, 13].s));
    assert(y.length == 2);
    assert(y.contains(12));
    assert(y.contains(13));
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
    alias K = uint;
    alias X = HashMapOrSet!(K, void, null, FNV!(64, true));

    auto x = X.withElements([12, 13].s);
    auto y = X.withElements([10, 12, 13, 15].s);
    y.intersectWith(x);
    assert(y.length == 2);
    assert(y.contains(12));
    assert(y.contains(13));
}

/// Returns forward range that iterates through the elements of `c`.
auto byElement(HashMapOrSetType)(auto ref inout(HashMapOrSetType) c)
    @trusted
    if (isInstanceOf!(HashMapOrSet,
                      HashMapOrSetType))
{
    alias C = const(HashMapOrSetType);
    static if (__traits(isRef, c))
    {
        auto result = C.ByLvalueElement!C((C.LvalueElementRef!C(cast(C*)&c)));
        result.initFirstNonEmptyBin();
        return result;
    }
    else
    {
        import std.algorithm.mutation : move;
        auto result = C.ByRvalueElement!C((C.RvalueElementRef!C(move(*(cast(HashMapOrSetType*)&c))))); // reinterpret
        result.initFirstNonEmptyBin();
        return move(result);
    }
}

@safe:

/// make range from l-value and r-value. element access is always const
pure nothrow @nogc unittest
{
    alias K = uint;
    alias X = HashMapOrSet!(K, void, null, FNV!(64, true));

    immutable a = [11, 22].s;

    // mutable
    auto x = X.withElements(a);
    foreach (e; x.byElement)    // from l-value
    {
        static assert(is(typeof(e) == const(K))); // always const access
    }

    // const
    const y = X.withElements(a);
    foreach (e; y.byElement)    // from l-value
    {
        static assert(is(typeof(e) == const(K)));
    }

    foreach (e; X.withElements([11].s).byElement) // from r-value
    {
        static assert(is(typeof(e) == const(K))); // always const access
    }
}

/// test various things
pure nothrow @nogc unittest
{
    immutable n = 600;

    alias K = uint;

    import std.meta : AliasSeq;
    foreach (V; AliasSeq!(void, string))
    {
        alias X = HashMapOrSet!(K, V, null, FNV!(64, true));

        static if (!X.hasValue)
        {
            auto x = X.withElements([11, 12, 13].s);

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

            {
                auto xc = X.withElements([11, 12, 13].s);
                assert(xc.length == 3);
                assert(xc.contains(11));

                // TODO http://forum.dlang.org/post/kvwrktmameivubnaifdx@forum.dlang.org
                xc.resetAllMatching!(_ => _ == 11);

                assert(xc.length == 2);
                assert(!xc.contains(11));

                xc.resetAllMatching!(_ => _ == 12);
                assert(!xc.contains(12));
                assert(xc.length == 1);

                xc.resetAllMatching!(_ => _ == 13);
                assert(!xc.contains(13));
                assert(xc.length == 0);

                // this is ok
                foreach (e; xc.byElement) {}

            }

            {
                auto k = X.withElements([11, 12].s).filtered!(_ => _ != 11).byElement;
                static assert(isInputRange!(typeof(k)));
                assert(k.front == 12);
                k.popFront();
                assert(k.empty);
            }

            {
                X q;
                auto qv = [11U, 12U, 13U, 14U].s;
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

    t.clear();
}

/// class as value
@trusted pure unittest
{
    import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError;

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
    struct K
    {
        @disable this(this);
        uint value;
    }

    class V
    {
        this(uint data) { this.data = data; }
        uint data;
    }

    alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
    auto x = X();

    x[K(42)] = new V(43);

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
    import std.algorithm.comparison : equal;
    import digestx.fnv : FNV;
    import array_help : s;
}
