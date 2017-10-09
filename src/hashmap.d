module hashmap;

import container_traits;

enum InsertionStatus { added, modified, unchanged }

/** Hash set (or map) storing (key) elements of type `K` and values of type `V`.
 *
 * Uses small-size-optimized (SSO) arrays as buckets.
 *
 * Params:
 *      K = key type.
 *      V = value type.
 *      Allocator = memory allocator.
 *      hasher = hash function or std.digest Hash.
 *      smallBucketMinCapacity = minimum capacity of small bucket
 *
 * TODO implement bucket growth
 *
 * TODO rehash: if allocator has realloc we can do rehashing in-place similar to
 * reordering in in-place radix (integer_sorting.d), otherwise rehash into new
 * copy of buckets and free old buckets when done
 *
 * TODO forward-ranges `byValue`, `byKeyValue`
 *
 * TODO support uncopyable value type for map-case
 *
 * TODO benchmark against https://github.com/greg7mdp/sparsepp
 *
 * TODO use https://dlang.org/phobos/std_experimental_allocator.html:
 * struct HashTable
 * {
 *     private IAllocator _allocator;
 *     this(size_t buckets, IAllocator allocator = theAllocator) {
 *         this._allocator = allocator;
 *         ...
 *     }
 *     // Getter and setter
 *     IAllocator allocator() { return _allocator; }
 *     void allocator(IAllocator a) { assert(empty); _allocator = a; }
 * }
 * EMSI-containers has some helper logic for this.
 */
struct HashMapOrSet(K, V = void,
                    alias Allocator = null,
                    alias hasher = hashOf,
                    uint smallBucketMinCapacity = 1)
    if (smallBucketMinCapacity >= 1) // no use having empty small buckets
{
    import std.traits : hasElaborateDestructor;
    import std.algorithm.mutation : move, moveEmplace;
    import std.algorithm.searching : canFind, countUntil;
    import hash_ex : HashOf;

    /** In the hash map case, `V` is non-void, and a value is stored alongside
     * the key of type `K`.
     */
    enum hasValue = !is(V == void);

    /** Type of key stored. */
    alias KeyType = K;

    /// Element type.
    static if (hasValue)
    {
        struct T
        {
            K key;
            V value;
        }

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref inout(T) element)
        {
            return element.key;
        }

        /// Get reference to key part of `element`.
        static ref inout(K) keyRefOf()(ref inout(T) element)
        {
            return element.key;
        }

        /// Get value part of element.
        static auto ref inout(V) valueOf()(auto ref inout(T) element)
        {
            return element.value;
        }

        alias ValueType = V;
    }
    else                        // HashSet
    {
        private alias T = K;

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref inout(T) element)
        {
            return element;
        }

        /// Get reference to key part of `element`.
        static ref inout(K) keyRefOf()(ref inout(T) element)
        {
            return element;
        }
    }

    alias ElementType = T;

    /** Make with room for storing at least `capacity` number of elements.
     */
    pragma(inline)              // LDC can, DMD cannot inline
    static typeof(this) withCapacity(size_t capacity)
    {
        return typeof(return)(capacity);
    }

    /** Construct with room for storing at least `capacity` number of elements.
     */
    private this(size_t capacity)
    {
        const minimumBucketCount = capacity / smallBucketCapacity;
        import std.math : nextPow2;

        // make bucket count a power of two
        immutable bucketCount = nextPow2(minimumBucketCount == 0 ?
                                         0 :
                                         minimumBucketCount - 1);

        // initialize buckets
        _buckets = Buckets.withLength(bucketCount);
        _largeBucketFlags = LargeBucketFlags.withLength(bucketCount);
        _length = 0;
    }

    /// Destruct.
    ~this()
    {
        release();
    }

    @disable this(this);

    /// Duplicate.
    typeof(this) dup() @trusted
    {
        typeof(return) that;

        that._buckets.reserve(_buckets.length);
        that._buckets.length = _buckets.length; // TODO this zero-initializes before initialization below, use unsafe setLengthOnlyUNSAFE
        foreach (immutable bucketIndex; 0 .. _buckets.length)
        {
            import std.conv : emplace;
            if (_largeBucketFlags[bucketIndex])
            {
                emplace!(LargeBucket)(&that._buckets[bucketIndex].large, _buckets[bucketIndex].large[]);
            }
            else
            {
                emplace!(SmallBucket)(&that._buckets[bucketIndex].small, _buckets[bucketIndex].small);
            }
        }

        that._largeBucketFlags = _largeBucketFlags.dup;
        that._length = _length;
        return that;
    }

    void grow()
    {
        const newBucketCount = bucketCount << 2;
        auto copy = typeof(this).withCapacity(newBucketCount);
        dln("TODO");
    }

    /// Equality.
    bool opEquals(in ref typeof(this) rhs) const @trusted
    {
        if (_length != rhs._length) { return false; }
        foreach (immutable bucketIndex; 0 .. _buckets.length)
        {
            foreach (const ref element; bucketElementsAt(bucketIndex))
            {
                if (!rhs.contains(element)) { return false; }
            }
        }
        return true;
    }

    /// Empty.
    void clear()
    {
        release();
        resetInternalData();
    }

    /// Release internal store.
    private void release() @trusted
    {
        foreach (immutable bucketIndex; 0 .. _buckets.length)
        {
            if (_largeBucketFlags[bucketIndex])
            {
                static if (hasElaborateDestructor!LargeBucket)
                {
                    .destroy(_buckets[bucketIndex].large);
                }
            }
            else
            {
                /* TODO SmallBucket itself doens't need to be destroyed only
                   it's elements and gc_removeRange doesn't need to be called
                   either, that is take car of by dtor of _buckets. */
                static if (hasElaborateDestructor!SmallBucket)
                {
                    .destroy(_buckets[bucketIndex].small);
                }
            }
        }
    }

    /// Reset internal data.
    private void resetInternalData()
    {
        _buckets.clear();
        _largeBucketFlags.clear();
        _length = 0;
    }

    /** Check if `element` is stored.
        Returns: `true` if element was already present, `false` otherwise.
     */
    bool contains(in T element) const @trusted
    {
        immutable bucketIndex = keyToIndex(keyRefOf(element));
        return bucketElementsAt(bucketIndex).canFind(element);
    }

    /** Insert `element`, being either a key, value (map-case) or a just a key (set-case).
     */
    InsertionStatus insert(T element) @trusted
    {
        immutable bucketIndex = keyToIndex(keyRefOf(element));
        T[] bucketElements = bucketElementsAt(bucketIndex);

        // find element offset matching key
        static if (hasValue)
        {
            immutable ptrdiff_t elementOffset = bucketElements.countUntil!((a, b) => (a.key == b))(keyOf(element));
        }
        else
        {
            immutable ptrdiff_t elementOffset = bucketElements.countUntil(element);
        }

        if (elementOffset != -1) // hit
        {
            static if (hasValue) // replace value
            {
                if (bucketElements[elementOffset].value != valueOf(element))
                {
                    bucketElements[elementOffset].value = valueOf(element); // replace valae
                    return typeof(return).modified;
                }
            }
            return typeof(return).unchanged;
        }
        else                    // no hit
        {
            if (_largeBucketFlags[bucketIndex])
            {
                _buckets[bucketIndex].large.insertBackMove(element);
                _length += 1;
            }
            else
            {
                immutable ok = _buckets[bucketIndex].small.insertBackMaybe(element);
                if (!ok)        // if full
                {
                    import std.conv : emplace;
                    // expand small to large
                    SmallBucket small = _buckets[bucketIndex].small;
                    emplace!(LargeBucket)(&_buckets[bucketIndex].large, small[]);
                    _buckets[bucketIndex].large.insertBackMove(element);
                    _largeBucketFlags[bucketIndex] = true; // bucket is now large
                }
                _length += 1;
            }
            return typeof(return).added;
        }
    }

    /** Element reference (and in turn range iterator). */
    static private struct ElementRef
    {
        HashMapOrSet* table;
        size_t bucketIndex;     // index to bucket inside table
        size_t elementOffset;   // offset to element inside bucket

        bool opCast(T : bool)() const
        {
            return cast(bool)table;
        }

        ref inout(T) opUnary(string s)() inout
            if (s == "*")
        {
            return table.bucketElementsAt(bucketIndex)[elementOffset];
        }
    }

    /// ditto
    static if (!hasValue)       // HashSet
    {
        bool opBinaryRight(string op)(in K key) inout @trusted
            if (op == "in")
        {
            return contains(key);
        }
    }

    static if (hasValue)        // HashMap
    {
        scope inout(ElementRef) opBinaryRight(string op)(in K key) inout @trusted
            if (op == "in")
        {
            immutable bucketIndex = keyToIndex(key);
            immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIndex).countUntil!(_ => _.key == key); // TODO functionize
            if (elementOffset != -1) // hit
            {
                return typeof(return)(&this, bucketIndex, elementOffset);
            }
            else                    // miss
            {
                return typeof(return).init;
            }
        }

        static private struct ByKey // TODO scope
        {
            @property bool empty() const
            {
                return _eRef.bucketIndex == _eRef.table.bucketCount;
            }

            @property auto front() inout
            {
                return (*_eRef).key;
            }

            void popFront()
            {
                assert(!empty);
                _eRef.elementOffset += 1; // next element
                // if current bucket was emptied
                while (_eRef.elementOffset >= _eRef.table.bucketElementsAt(_eRef.bucketIndex).length)
                {
                    // next bucket
                    _eRef.bucketIndex += 1;
                    _eRef.elementOffset = 0;
                    if (empty) { break; }
                }
            }

            @property typeof(this) save() // ForwardRange
            {
                return this;
            }

            private ElementRef _eRef;  // range iterator
        }

        /// Returns forward range that iterates through the keys.
        inout(ByKey) byKey() inout
        {
            return typeof(return)(inout(ElementRef)(&this));
        }

        /// Indexing.
        ref inout(V) opIndex(in K key) inout
        {
            immutable bucketIndex = keyToIndex(key);
            immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIndex).countUntil!(_ => _.key == key); // TODO functionize
            if (elementOffset != -1) // hit
            {
                return bucketElementsAt(bucketIndex)[elementOffset].value;
            }
            else                    // miss
            {
		import std.conv : text;
                import core.exception : RangeError;
                throw new RangeError("Key " ~ text(key) ~ " not in table");
            }
        }

        /// Get value of `key` or `defaultValue` if `key` not present.
        inout(V) get(in K key, V defaultValue) inout // TODO make it return a ref. TODO make defaultValue lasy
        {
            immutable bucketIndex = keyToIndex(key);
            immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIndex).countUntil!(_ => _.key == key); // TODO functionize
            if (elementOffset != -1) // hit
            {
                return bucketElementsAt(bucketIndex)[elementOffset].value;
            }
            else                    // miss
            {
                return defaultValue;
            }
        }

	/** Supports $(B aa[key] = value;) syntax.
	 */
	void opIndexAssign(V value, K key)
	{
            insert(T(key, value));
	}

    }

    /** Remove `element` and, when possible, shrink its large bucket to small.

        Returns: `true` if element was removed, `false` otherwise.
    */
    bool remove(in K key)
        @trusted
    {
        immutable bucketIndex = keyToIndex(key);
        import container_algorithm : popFirst;
        if (_largeBucketFlags[bucketIndex])
        {
            static if (hasValue)
            {
                immutable hit = _buckets[bucketIndex].large.popFirst!"a.key == b"(key);
            }
            else
            {
                immutable hit = _buckets[bucketIndex].large.popFirst(key);
            }
            _length -= hit ? 1 : 0;
            if (hit &&
                _buckets[bucketIndex].large.length <= smallBucketCapacity) // large fits in small
            {
                auto small = SmallBucket.fromValuesUnsafe(_buckets[bucketIndex].large[]); // TODO move elements
                assert(small == _buckets[bucketIndex].large[]);

                .destroy(_buckets[bucketIndex].large);
                moveEmplace(small, _buckets[bucketIndex].small);

                _largeBucketFlags[bucketIndex] = false; // now small
                assert(_largeBucketFlags[bucketIndex] == false);
            }
            return hit;
        }
        else
        {
            static if (hasValue)
            {
                immutable hit = _buckets[bucketIndex].small.popFirst!"a.key == b"(key);
            }
            else
            {
                immutable hit = _buckets[bucketIndex].small.popFirst(key);
            }
            _length -= hit ? 1 : 0;
            return hit;
        }
    }

    /** Rehash.
     *
     * Reorganize `this` in place so that lookups are more efficient.
     */
    ref typeof(this) rehash()() @trusted
    {
        static assert(false, "TODO remove template parens of this functions and implement");
        // return this;
    }

    /// Check if empty.
    @property bool empty() const { return _length == 0; }

    /// Get length (read-only).
    @property size_t length() const { return _length; }

    /// Get bucket count.
    @property size_t bucketCount() const { return _buckets.length; }

    /// Bucket count statistics.
    struct BucketCounts
    {
        size_t smallCount;      // number of hybrid buckets being small
        size_t largeCount;      // number of hybrid buckets being large
    }

    /// Get bucket count statistics.
    BucketCounts bucketCounts() const
    {
        immutable largeCount = _largeBucketFlags.countOnes;
        immutable smallCount = _largeBucketFlags.length - largeCount;
        auto result = typeof(return)(smallCount,
                                     largeCount);
        assert(result.largeCount + result.smallCount == _largeBucketFlags.length);
        return result;
    }

    /** Retursn: elements in bucket at `bucketIndex`. */
    pragma(inline, true)
    private scope inout(T)[] bucketElementsAt(size_t bucketIndex) inout return
    {
        if (_largeBucketFlags[bucketIndex])
        {
            return _buckets[bucketIndex].large[];
        }
        else
        {
            return _buckets[bucketIndex].small[];
        }
    }

private:
    import basic_uncopyable_array : Array = UncopyableArray;
    import bitarray : BitArray;

    alias LargeBucket = Array!(T, Allocator);

    import std.algorithm : max;
    enum smallBucketCapacity = max(smallBucketMinCapacity,
                                   (LargeBucket.sizeof - 1) / T.sizeof);

    import arrayn : ArrayN;
    alias SmallBucket = ArrayN!(T, smallBucketCapacity);

    /** Small-size-optimized bucket array.
        Size-state (small or large) is determined corresponding bit in `LargeBucketFlags`.
     */
    union HybridBucket
    {
        SmallBucket small;
        LargeBucket large;
    }

    alias Buckets = Array!(HybridBucket, Allocator);
    alias LargeBucketFlags = BitArray!(Allocator);

    Buckets _buckets;

    // TODO this store currently wastes 1 or 2 words as _bucket already contain
    // same _length and _store. Use MultiArray!(HybridBucket, bool) container to
    // store this.
    LargeBucketFlags _largeBucketFlags;

    size_t _length;

    /** Returns: bucket index of `hash`. */
    pragma(inline, true)
    size_t hashToIndex(size_t hash) const
    {
        const size_t mask = _buckets.length - 1;
        assert((~mask ^ mask) == size_t.max); // assert that _buckets.length is a power of 2
        return hash & mask;
    }

    /** Returns: bucket index of `key`. */
    pragma(inline, true)
    size_t keyToIndex()(in auto ref K key) const
    {
        return hashToIndex(HashOf!(hasher)(key));
    }

}

alias HashSet(K,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBucketMinCapacity = 1) = HashMapOrSet!(K, void,
                                                               Allocator,
                                                               hasher,
                                                               smallBucketMinCapacity);

alias HashMap(K, V,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBucketMinCapacity = 1) = HashMapOrSet!(K, V,
                                                               Allocator,
                                                               hasher,
                                                               smallBucketMinCapacity);

@safe pure nothrow @nogc unittest
{
    import digestx.fnv : FNV;

    immutable n = 11;

    alias K = uint;

    foreach (V; AliasSeq!(void, string))
    {
        alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
        auto x1 = X.withCapacity(n);

        // all buckets start small
        assert(x1.bucketCounts.largeCount == 0);

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
                assert(x1.contains(e2));
                x1.remove(key);
                x1[key] = value;
            }

            assert(x1.length == key + 1);

            assert(key in x1);
            static if (X.hasValue)
            {
                assert(!x1.contains(X.ElementType(key, "_"))); // other value
            }

            assert(x1.insert(element) == InsertionStatus.unchanged);
            assert(x1.length == key + 1);

            assert(key in x1);
        }

        static if (X.hasValue)
        {
            import basic_uncopyable_array : Array = UncopyableArray;
            Array!(X.ElementType) a1;
            foreach (key; x1.byKey)
            {
                auto eRef = key in x1;
                assert(eRef);
                a1 ~= X.ElementType(key, (*eRef).value);
            }
            assert(x1.length == a1.length);
            foreach (element; a1[])
            {
                auto eRef = element.key in x1;
                assert(eRef);
                assert((*eRef).value == element.value);
            }
        }

        assert(x1.length == n);

        // duplicate x1

        auto x2 = x1.dup;
        assert(x1 == x2);
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

            auto hit = key in x1;
            assert(hit);
            static if (X.hasValue)
            {
                assert(*hit == element);
            }

            assert(x1.remove(key));
            assert(x1.length == n - key - 1);

            assert(key !in x1);
            assert(!x1.remove(key));
            assert(x1.length == n - key - 1);
        }

        assert(x1.bucketCounts.largeCount == 0);

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

        assert(x2.bucketCounts.largeCount == 0);

        assert(x2.length == 0);

        x2.clear();
        assert(x2.length == 0);
    }
}

/// range checking
pure unittest
{
    import digestx.fnv : FNV;

    immutable n = 11;

    alias K = uint;
    alias V = string;

    import std.exception : assertThrown, assertNotThrown;
    import core.exception : RangeError;

    alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
    auto s = X.withCapacity(n);

    static if (X.hasValue)
    {
        assertThrown!RangeError(s[0]);
        s[0] = V.init;
        assertNotThrown!RangeError(s[0]);
    }
}

// version(unittest)
// {
//     private static struct US
//     {
//         @disable this(this);
//         int x;
//     }
// }

// /// uncopyable element type
// pure unittest
// {
//     import digestx.fnv : FNV;

//     immutable n = 11;

//     alias K = US;
//     alias V = string;

//     import std.exception : assertThrown, assertNotThrown;
//     import core.exception : RangeError;

//     alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
//     auto s = X.withCapacity(n);

//     static if (X.hasValue)
//     {
//         assertThrown!RangeError(s[0]);
//         s[0] = V.init;
//         assertNotThrown!RangeError(s[0]);
//     }
// }

version = show;

version(unittest)
{
    import std.meta : AliasSeq;
    import array_help : s;
}

version(show)
{
    import dbgio;
}
