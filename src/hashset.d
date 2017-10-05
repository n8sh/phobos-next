module hashset;

import container_traits;

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
 * TODO extend to HashSetOrMap and specialize HashSet to HashMap with void
 * Value-type.
 *
 * TODO use https://dlang.org/phobos/std_experimental_allocator.html as:
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
struct HashSetOrMap(K, V = void,
                    alias Allocator = null,
                    alias hasher = hashOf,
                    uint smallBucketMinCapacity = 1)
    if (smallBucketMinCapacity >= 1) // no use having empty small buckets
{
    import std.algorithm.mutation : move, moveEmplace;
    import std.algorithm.searching : canFind;

    enum hasValue = !is(V == void);

    alias KeyType = K;

    /// Element type.
    static if (hasValue)        // HashMap
    {
        struct T
        {
            K key;
            V value;
        }

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref inout(T) elt)
        {
            return elt.key;
        }

        alias ValueType = V;
    }
    else                        // HashSet
    {
        private alias T = K;

        /// Get key part of element.
        static auto ref inout(K) keyOf()(auto ref inout(T) elt)
        {
            return elt;
        }
    }

    alias ElementType = T;

    /** Construct with room for storing `capacity` number of elements.
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
        immutable bucketCount = nextPow2(minimumBucketCount == 0 ? 0 : minimumBucketCount - 1);
        // so we can use fast bitmask instead of slower division remainder
        _hashMask = bucketCount - 1;
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

    /// Duplicate.
    typeof(this) dup()
    {
        typeof(return) that;
        that._buckets = _buckets.dup;
        that._largeBucketFlags = _largeBucketFlags.dup;
        that._hashMask = _hashMask;
        that._length = _length;
        return that;
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
                .destroy(_buckets[bucketIndex].large);
            }
            else
            {
                .destroy(_buckets[bucketIndex].small);
            }
        }
    }

    /// Reset internal data.
    private void resetInternalData()
    {
        _buckets.clear();
        _largeBucketFlags.clear();
        _hashMask = 0;
        _length = 0;
    }

    /** Insert `element`.
        Returns: `true` if element was already present, `false` otherwise (similar
        to behaviour of `contains`).
     */
    bool insert(T element) @trusted
    {
        import std.conv : emplace;
        immutable bucketIndex = bucketHash!(hasher)(keyOf(element)) & _hashMask;
        if (_largeBucketFlags[bucketIndex])
        {
            if (!_buckets[bucketIndex].large[].canFind(element))
            {
                _buckets[bucketIndex].large.insertBackMove(element);
                _length += 1;
                return false;
            }
        }
        else
        {
            if (!_buckets[bucketIndex].small[].canFind(element))
            {
                immutable ok = _buckets[bucketIndex].small.insertBackMaybe(element);
                if (!ok)        // if full
                {
                    // expand small to large
                    SmallBucket small = _buckets[bucketIndex].small;
                    emplace!(LargeBucket)(&_buckets[bucketIndex].large, small[]);
                    _buckets[bucketIndex].large.insertBackMove(element);
                    _largeBucketFlags[bucketIndex] = true; // bucket is now large
                }
                _length += 1;
                return false;
            }
        }
        return true;
    }

    /** Check if `element` is stored.
        Returns: `true` if element was already present, `false` otherwise.
     */
    bool contains(in T element) const @trusted
    {
        immutable bucketIndex = bucketHash!(hasher)(keyOf(element)) & _hashMask;
        if (_largeBucketFlags[bucketIndex])
        {
            return _buckets[bucketIndex].large[].canFind(element);
        }
        else
        {
            return _buckets[bucketIndex].small[].canFind(element);
        }
    }

    /// ditto
    bool opBinaryRight(string op)(in T element) const
        if (op == "in")
    {
        return contains(element); // TODO return entry reference instead
    }

    /** Remove `element` and, when possible, shrink its large bucket to small.

        Returns: `true` if element was removed, `false` otherwise.
     */
    bool remove(in T element)
        @trusted
    {
        immutable bucketIndex = bucketHash!(hasher)(keyOf(element)) & _hashMask;
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

    /// Check if empty.
    bool empty() const { return _length == 0; }

    /// Get length.
    @property size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    /// Get bucket count.
    @property size_t bucketCount() const { return _buckets.length; }

    /// Bucket count statistics.
    struct BucketCounts
    {
        size_t smallCount;      // number of hybrid bucket being small
        size_t largeCount;      // number of hybrid bucket being large
    }

    /// Get bucket count statistics.
    BucketCounts bucketCounts() const
    {
        const largeCount = _largeBucketFlags.countOnes;
        const smallCount = _largeBucketFlags.length - largeCount;
        auto result = typeof(return)(smallCount,
                                     largeCount);
        assert(result.largeCount + result.smallCount == _largeBucketFlags.length);
        return result;
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

    size_t _hashMask;
}

alias HashSet(K,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBucketMinCapacity = 1) = HashSetOrMap!(K, void,
                                                               Allocator,
                                                               hasher,
                                                               smallBucketMinCapacity);

alias HashMap(K, V,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBucketMinCapacity = 1) = HashSetOrMap!(K, V,
                                                               Allocator,
                                                               hasher,
                                                               smallBucketMinCapacity);

/** Get index into `bucket` for `key`.
 */
pragma(inline)              // LDC can inline, DMD cannot
size_t bucketHash(alias hasher, K)(in K key)
{
    import std.digest.digest : isDigest;
    import std.traits : hasMember;

    static if (__traits(compiles, { size_t _ = hasher(key); }))
    {
        return hasher(key);
    }
    else static if (__traits(compiles, { enum _ = isDigest!hasher; }) &&
                    isDigest!hasher &&
                    hasMember!(hasher, "get"))
    {
        import std.digest.digest : makeDigest;

        auto dig = makeDigest!(hasher);
        dig.put((cast(ubyte*)&key)[0 .. key.sizeof]);
        dig.finish();

        static if (is(typeof(dig.get()) == typeof(return)))
        {
            return dig.get();
        }
        else static if (is(typeof(dig.get()) == typeof(return)[2]))
        {
            return (dig.get()[0] ^ dig.get()[1]);
        }
        else
        {
            static assert(0, "Handle get() with return type " ~ typeof(dig.get()).stringof ~
                          " on " ~ size_t.sizeof.stringof ~ "-bit platform");
        }
    }
    else static if (__traits(compiles, { auto _ = hasher((ubyte[]).init); }))
    {
        // cast input `key` to `ubyte[]` and use std.digest API
        immutable digest = hasher((cast(ubyte*)&key)[0 .. key.sizeof]); // TODO ask forums when this isn't correct

        static assert(digest.sizeof >=
                      typeof(return).sizeof,
                      `Size of digest is ` ~ digest.sizeof
                      ~ ` but needs to be at least ` ~ typeof(return).sizeof.stringof);

        import std.traits : isUnsigned, isStaticArray;

        static if (isUnsigned!(typeof(digest)))
        {
            return cast(typeof(return))digest; // fast modulo calculation
        }
        else static if (isStaticArray!(typeof(digest)))
        {
            typeof(return) hashIndex;

            static if (2*size_t.sizeof == digest.sizeof)
            {
                // for instance, use all 128-bits when size_t is 64-bit
                (cast(ubyte*)&hashIndex)[0 .. hashIndex.sizeof] = (digest[0 .. hashIndex.sizeof] ^
                                                                   digest[hashIndex.sizeof .. 2*hashIndex.sizeof]);
            }
            else
            {
                (cast(ubyte*)&hashIndex)[0 .. hashIndex.sizeof] = digest[0 .. hashIndex.sizeof];
            }

            return hashIndex;
        }
        else
        {
            static assert(0, "Unsupported digest type " ~ typeof(digest).stringof);
        }
    }
    else
    {
        static assert(0, "Cannot combine hasher " ~ hasher.stringof ~
                      " with element type " ~ K.stringof);
    }
}

@safe pure nothrow @nogc unittest
{
    immutable n = 11;
    alias K = uint;

    import digestx.fnv : FNV;
    auto s = HashSet!(K, null, FNV!(64, true)).withCapacity(n);

    // all buckets start small
    assert(s.bucketCounts.smallCount != 0);
    assert(s.bucketCounts.largeCount == 0);

    foreach (immutable i; 0 .. n)
    {
        assert(i !in s);

        assert(s.length == i);
        assert(!s.insert(i));
        assert(s.length == i + 1);

        assert(i in s);

        assert(s.insert(i));
        assert(s.length == i + 1);

        assert(i in s);
    }

    assert(s.length == n);

    foreach (immutable i; 0 .. n)
    {
        assert(s.length == n - i);

        assert(i in s);

        assert(s.remove(i));
        assert(s.length == n - i - 1);

        assert(i !in s);
        assert(!s.remove(i));
        assert(s.length == n - i - 1);
    }

    assert(s.bucketCounts.largeCount == 0);

    assert(s.length == 0);

    s.clear();
    assert(s.length == 0);
}

@safe pure nothrow @nogc unittest
{
    immutable n = 11;
    alias K = uint;
    alias V = string;

    import digestx.fnv : FNV;
    auto s = HashMap!(K, V, null, FNV!(64, true)).withCapacity(n);

    // all buckets start small
    assert(s.bucketCounts.smallCount != 0);
    assert(s.bucketCounts.largeCount == 0);
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
