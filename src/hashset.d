module hashset;

/** Hash set storing elements of type `T`.

    Uses small-size-optimized (SSO) arrays as buckets.
 */
struct HashSet(T,
               alias Allocator = null,
               alias hashFunction = hashOf)
{
    /** Construct with room for storing `capacity` number of elements.
     */
    static typeof(this) withCapacity(size_t capacity)
    {
        typeof(return) that;
        that.initialize(capacity / smallBucketLength);
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
        hashMask = bucketCount - 1;
        initializeBuckets(bucketCount);
    }

    /** Initialize `bucketCount` number of buckets.
     */
    private void initializeBuckets(size_t bucketCount) @trusted // TODO remove @trusted
    {
        _buckets = Buckets.withLength(bucketCount);
        _largeBucketFlags = LargeBucketFlags.withLength(bucketCount);
    }

    /// Destruct.
    ~this()
    {
        release();
    }

    /// Empty.
    void clear() @safe nothrow
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
        assert(false, "TODO");
    }

    /** Insert `value`.
        Returns: `true` if value was already present, `false` otherwise. This is
        similar to behaviour of `contains`.
     */
    bool insert(T value) @trusted
    {
        import std.conv : emplace;
        import std.algorithm.searching : canFind;
        immutable bucketIndex = bucketHashIndex(value);
        if (_largeBucketFlags[bucketIndex])
        {
            if (!_buckets[bucketIndex].large[].canFind(value))
            {
                _buckets[bucketIndex].large.insertBackMove(value);
                return false;
            }
        }
        else
        {
            if (!_buckets[bucketIndex].small[].canFind(value))
            {
                immutable ok = _buckets[bucketIndex].small.insertBackMaybe(value);
                if (!ok)        // if full
                {
                    // expand small to large
                    SmallBucket smallCopy = _buckets[bucketIndex].small;
                    emplace!(LargeBucket)(&_buckets[bucketIndex].large, smallCopy[]);
                    _buckets[bucketIndex].large.insertBackMove(value);
                    _largeBucketFlags[bucketIndex] = true; // bucket is now large
                }
                return false;
            }
        }
        return true;
    }

    /** Check if `value` is stored.
        Returns: `true` if value was already present, `false` otherwise.
     */
    bool contains(in T value) const @trusted
    {
        import std.algorithm.searching : canFind;
        immutable bucketIndex = bucketHashIndex(value);
        if (_largeBucketFlags[bucketIndex])
        {
            return _buckets[bucketIndex].large[].canFind(value);
        }
        else
        {
            return _buckets[bucketIndex].small[].canFind(value);
        }
    }

    /** Remove `value`.
        Returns: `true` if values was removed, `false` otherwise.
     */
    bool remove(in T value)
    {
        import std.algorithm.searching : find;
        immutable bucketIndex = bucketHashIndex(value);
        assert(0, "TODO Implement removeAtIndex in Array and use _buckets[bucketIndex].removeAtIndex() here");
        assert(0, "TODO Check shrinkage to SmallBucket");
    }

    /** Get index into `bucket` for `value`.
     */
    pragma(inline, true)
    size_t bucketHashIndex(in T value) const
    {
        static if (__traits(compiles, { typeof(return) _ = hashFunction(value); }))
        {
            return hashFunction(value) & hashMask; // TODO is this correct?
        }
        else
        {
            // cast input `value` to `ubyte[]` and use std.digest API
            immutable digest = hashFunction((cast(ubyte*)&value)[0 .. value.sizeof]); // TODO ask forums when this isn't correct

            static assert(digest.sizeof >=
                          typeof(return).sizeof,
                          `Size of digest is ` ~ digest.sizeof
                          ~ ` but needs to be at least ` ~ typeof(return).sizeof.stringof);

            import std.traits : isUnsigned, isStaticArray;

            static if (isUnsigned!(typeof(digest)))
            {
                return cast(typeof(return))digest & hashMask; // fast modulo calculation
            }
            else static if (isStaticArray!(typeof(digest)))
            {
                typeof(return) hashIndex = void;
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
                return hashIndex & hashMask;
            }
            else
            {
                static assert(0, "Unsupported return value of hash function");
            }
        }
    }

private:
    import basic_uncopyable_array : Array = UncopyableArray; // TODO change to CopyableArray when
    import basic_bitarray : BitArray;

    alias LargeBucket = Array!(T, Allocator);

    import std.algorithm : max;
    enum smallBucketLength = max(1, // at least one element in small bucket for good performance
                                 (LargeBucket.sizeof - 1) / T.sizeof);

    import arrayn : ArrayN;
    alias SmallBucket = ArrayN!(T, smallBucketLength);

    /** Small-size-optimized bucket. */
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

    size_t hashMask;
}

@safe pure nothrow unittest
{
    immutable elementCount = 2^^10;
    alias T = uint;
    auto s = HashSet!(T, null).withCapacity(elementCount);
    foreach (immutable i; 0 .. elementCount)
    {
        assert(!s.contains(i));
        assert(!s.insert(i));
        assert(s.contains(i));
        assert(s.insert(i));
        assert(s.contains(i));
    }
}

// version = show;

version(show)
@safe nothrow /*pure @nogc*/ unittest
{
    immutable elementCount = 2^^10;
    foreach (immutable i; 0 .. elementCount)
    {
        dln(typeidHashOf(i));   // doesn't work just prints 0, 1, 2, ...
    }
}

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio : dln;
}
