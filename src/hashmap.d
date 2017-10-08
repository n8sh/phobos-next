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
 *
 * TODO merge with EMSI containers.hashmap
 * TODO implement bucket growth
 * TODO forward-ranges `byKey`, `byValue`, `byKeyValue`
 * TODO benchmark against https://github.com/greg7mdp/sparsepp
 *
 * TODO use https://dlang.org/phobos/std_experimental_allocator.html:
 *
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
 *
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

        /// Get value part of element.
        static auto ref inout(V
            ) valueOf()(auto ref inout(T) element)
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
    }

    alias ElementType = T;

    /** Construct with room for storing at least `capacity` number of elements.
     */
    static typeof(this) withCapacity(size_t capacity)
    {
        typeof(return) that;
        that.initialize(capacity / smallBucketCapacity);
        return that;
    }

    /** Initialize at least `minimumBucketCount` number of initial buckets.
     */
    private void initialize(size_t minimumBucketCount)
    {
        import std.math : nextPow2;
        // make bucket count a power of two
        immutable bucketCount = nextPow2(minimumBucketCount == 0 ?
                                         0 :
                                         minimumBucketCount - 1);
        // so we can use fast bitmask instead of slower division remainder
        initializeBuckets(bucketCount);
    }

    /** Initialize `bucketCount` number of buckets.
     */
    private void initializeBuckets(size_t bucketCount) @trusted
    {
        _buckets = Buckets.withLength(bucketCount);
        _largeBucketFlags = LargeBucketFlags.withLength(bucketCount);
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

    /** Insert `element`.
     */
    InsertionStatus insert(T element) @trusted
    {
        immutable bucketIndex = hashToIndex(HashOf!(hasher)(keyOf(element)));
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

    /** Check if `element` is stored.
        Returns: `true` if element was already present, `false` otherwise.
     */
    bool contains(in T element) const @trusted
    {
        immutable bucketIndex = hashToIndex(HashOf!(hasher)(keyOf(element)));
        return bucketElementsAt(bucketIndex).canFind(element);
    }

    /** Reference to element. */
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
    scope inout(ElementRef) opBinaryRight(string op)(in T element) inout @trusted
        if (op == "in")
    {
        immutable bucketIndex = hashToIndex(HashOf!(hasher)(keyOf(element)));
        immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIndex).countUntil(element);
        if (elementOffset != -1) // hit
        {
            return typeof(return)(&this, bucketIndex, elementOffset);
        }
        else                    // miss
        {
            return typeof(return).init;
        }
    }

    static if (hasValue)
    {
        /// Indexing.
        ref inout(V) opIndex(in K key) inout
        {
            immutable bucketIndex = hashToIndex(HashOf!(hasher)(key));
            immutable ptrdiff_t elementOffset = bucketElementsAt(bucketIndex).countUntil!(_ => _.key == key);
            if (elementOffset != -1) // hit
            {
                return bucketElementsAt(bucketIndex)[elementOffset].value;
            }
            else                    // miss
            {
		import std.conv : text;
                throw new Exception("Key " ~ text(key) ~ " not in table");
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
    bool remove(in T element)
        @trusted
    {
        immutable bucketIndex = hashToIndex(HashOf!(hasher)(keyOf(element)));
        import container_algorithm : popFirst;
        if (_largeBucketFlags[bucketIndex])
        {
            immutable hit = _buckets[bucketIndex].large.popFirst(element);
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
            immutable hit = _buckets[bucketIndex].small.popFirst(element);
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
    bool empty() const { return _length == 0; }

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
        return hash & (_buckets.length - 1); // assumes `_buckets.length` to be a power of 2
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
    import std.meta : AliasSeq;

    foreach (V; AliasSeq!(void, string))
    {
        alias X = HashMapOrSet!(K, V, null, FNV!(64, true));
        auto s1 = X.withCapacity(n);

        // all buckets start small
        assert(s1.bucketCounts.smallCount != 0);
        assert(s1.bucketCounts.largeCount == 0);

        // fill s1

        foreach (immutable i; 0 .. n)
        {
            static if (X.hasValue)
            {
                const v = "";
                const e = X.ElementType(i, v);
            }
            else
            {
                const e = i;
            }

            assert(e !in s1);

            assert(s1.length == i);
            assert(s1.insert(e) == InsertionStatus.added);

            static if (X.hasValue)
            {
                assert(s1.insert(X.ElementType(i, "a")) == InsertionStatus.modified);
                s1.remove(e);
                s1[i] = v;
            }

            assert(s1.length == i + 1);

            assert(e in s1);
            static if (X.hasValue)
            {
                assert(!s1.contains(X.ElementType(i, "_"))); // other value
            }

            assert(s1.insert(e) == InsertionStatus.unchanged);
            assert(s1.length == i + 1);

            assert(e in s1);
        }

        assert(s1.length == n);

        // duplicate s1

        auto s2 = s1.dup;
        assert(s1 == s2);
        static assert(!__traits(compiles, { const _ = s1 < s2; })); // no ordering
        assert(s2.length == n);

        // empty s1

        foreach (immutable i; 0 .. n)
        {
            static if (X.hasValue)
            {
                const e = X.ElementType(i, "");
            }
            else
            {
                const e = i;
            }

            assert(s1.length == n - i);

            auto hit = e in s1;
            assert(hit);
            assert(*hit == e);

            assert(s1.remove(e));
            assert(s1.length == n - i - 1);

            assert(e !in s1);
            assert(!s1.remove(e));
            assert(s1.length == n - i - 1);
        }

        assert(s1.bucketCounts.largeCount == 0);

        assert(s1.length == 0);

        s1.clear();
        assert(s1.length == 0);

        // empty s2

        assert(s2.length == n); // should be not affected by emptying of s1

        foreach (immutable i; 0 .. n)
        {
            static if (X.hasValue)
            {
                const e = X.ElementType(i, "");
            }
            else
            {
                const e = i;
            }

            assert(s2.length == n - i);

            assert(e in s2);

            assert(s2.remove(e));
            assert(s2.length == n - i - 1);

            assert(e !in s2);
            assert(!s2.remove(e));
            assert(s2.length == n - i - 1);
        }

        assert(s2.bucketCounts.largeCount == 0);

        assert(s2.length == 0);

        s2.clear();
        assert(s2.length == 0);
    }
}

version = show;

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio;
}
