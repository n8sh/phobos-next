module hashset;

/** Hash set storing elements of type `T`.

    Uses small-size-optimized (SSO) arrays as buckets.
 */
struct HashSet(T,
               alias Allocator = null,
               alias hasher = hashOf)
{
    import std.algorithm.mutation : move, moveEmplace;
    import std.algorithm.searching : canFind;

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
        _hashMask = bucketCount - 1;
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
        _buckets.clear();
        _largeBucketFlags.clear();
        _hashMask = 0;
        _length = 0;
    }

    /** Insert `value`.
        Returns: `true` if value was already present, `false` otherwise (similar
        to behaviour of `contains`).
     */
    bool insert(T value) @trusted
    {
        import std.conv : emplace;
        immutable bucketIndex = bucketHashIndex(value);
        if (_largeBucketFlags[bucketIndex])
        {
            if (!_buckets[bucketIndex].large[].canFind(value)) // TODO optimize
            {
                _buckets[bucketIndex].large.insertBackMove(value);
                _length += 1;
                return false;
            }
        }
        else
        {
            if (!_buckets[bucketIndex].small[].canFind(value)) // TODO optimize
            {
                immutable ok = _buckets[bucketIndex].small.insertBackMaybe(value);
                if (!ok)        // if full
                {
                    // expand small to large
                    SmallBucket small = _buckets[bucketIndex].small;
                    emplace!(LargeBucket)(&_buckets[bucketIndex].large, small[]);
                    _buckets[bucketIndex].large.insertBackMove(value);
                    _largeBucketFlags[bucketIndex] = true; // bucket is now large
                }
                _length += 1;
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
        Returns: `true` if value was removed, `false` otherwise.
     */
    bool remove(in T value)
        @trusted
    {
        // dln("value:", value);
        immutable bucketIndex = bucketHashIndex(value);
        import container_algorithm : popFirst;
        if (_largeBucketFlags[bucketIndex])
        {
            const hit = _buckets[bucketIndex].large.popFirst(value);
            if (hit &&
                _buckets[bucketIndex].large.length <= smallBucketLength) // large fits in small
            {
                // auto small = SmallBucket.fromValuesUnsafe(_buckets[bucketIndex].large[]); // TODO move elements
                // assert(small == _buckets[bucketIndex].large[]);

                // SmallBucket small2;
                // moveEmplace(small, small2);

                // dln(bucketIndex, ": ", small);
                // .destroy(_buckets[bucketIndex].large);
                // moveEmplace(small, _buckets[bucketIndex].small);

                // _largeBucketFlags[bucketIndex] = false; // now small
                _length -= 1;
                // dln("...");
            }
            return hit;
        }
        else
        {
            const hit = _buckets[bucketIndex].small.popFirst(value);
            if (hit)
            {
                _length -= 1;
            }
            return hit;
        }
    }

    /** Get index into `bucket` for `value`.
     */
    pragma(inline)              // LDC can inline, DMD cannot
    size_t bucketHashIndex(in T value) const
    {
        import std.digest.digest : isDigest;
        import std.traits : hasMember;
        static if (__traits(compiles, { size_t _ = hasher(value); }))
        {
            return hasher(value) & _hashMask;
        }
        else static if (__traits(compiles, { enum _ = isDigest!hasher; }) &&
                        isDigest!hasher &&
                        hasMember!(hasher, "get"))
        {
            import std.digest.digest : makeDigest;
            auto dig = makeDigest!(hasher);
            dig.put((cast(ubyte*)&value)[0 .. value.sizeof]);
            dig.finish();
            static if (is(typeof(dig.get()) == typeof(return)))
            {
                return dig.get() & _hashMask;
            }
            else static if (is(typeof(dig.get()) == typeof(return)[2]))
            {
                return (dig.get()[0] ^ dig.get()[1]) & _hashMask;
            }
            else
            {
                static assert(0, "Handle get() with return type " ~ typeof(dig.get()).stringof ~
                              " on " ~ size_t.sizeof.stringof ~ "-bit platform");
            }
        }
        else static if (__traits(compiles, { auto _ = hasher((ubyte[]).init); }))
        {
            // cast input `value` to `ubyte[]` and use std.digest API
            immutable digest = hasher((cast(ubyte*)&value)[0 .. value.sizeof]); // TODO ask forums when this isn't correct

            static assert(digest.sizeof >=
                          typeof(return).sizeof,
                          `Size of digest is ` ~ digest.sizeof
                          ~ ` but needs to be at least ` ~ typeof(return).sizeof.stringof);

            import std.traits : isUnsigned, isStaticArray;

            static if (isUnsigned!(typeof(digest)))
            {
                return cast(typeof(return))digest & _hashMask; // fast modulo calculation
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
                return hashIndex & _hashMask;
            }
            else
            {
                static assert(0, "Unsupported digest type " ~ typeof(digest).stringof);
            }
        }
        else
        {
            static assert(0, "Cannot combine hasher " ~ hasher.stringof ~
                          " with element type " ~ T.stringof);
        }
    }

    /// Check if empty.
    bool empty() const { return _length == 0; }

    /// Get length.
    @property size_t length() const { return _length; }
    alias opDollar = length;    /// ditto

    struct BucketCounts
    {
        size_t smallCount;
        size_t largeCount;
    }

    /// Get bucket count statistics.
    BucketCounts bucketCounts() const
    {
        typeof(return) result;
        foreach (const i; 0 .. length)
        {
            if (_largeBucketFlags[i])
            {
                result.largeCount += 1;
            }
            else
            {
                result.smallCount += 1;
            }
        }
        return result;
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

    size_t _length;

    size_t _hashMask;
}

@safe pure nothrow unittest
{
    immutable elementCount = 11;
    alias T = uint;

    auto s = HashSet!(T, null).withCapacity(elementCount);

    foreach (immutable i; 0 .. elementCount)
    {
        assert(!s.contains(i));

        assert(s.length == i);
        assert(!s.insert(i));
        assert(s.length == i + 1);

        assert(s.contains(i));

        assert(s.insert(i));
        assert(s.length == i + 1);

        assert(s.contains(i));
    }

    assert(s.length == elementCount);

    foreach (immutable i; 0 .. elementCount)
    {
        show!i;
        assert(s.length == elementCount - i);

        assert(s.contains(i));

        assert(s.remove(i));
        assert(s.length == elementCount - i - 1);

        assert(!s.contains(i));
        assert(!s.remove(i));
        assert(s.length == elementCount - i - 1);
    }

    assert(s.length == 0);

    s.clear();
    assert(s.length == 0);
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
