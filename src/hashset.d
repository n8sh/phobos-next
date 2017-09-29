module hashset;

import std.traits : isIntegral, isUnsigned;
import core.internal.hash : hashOf;

/** Hash set storing elements of type `T`.

    Uses small-size-optimized (SSO) arrays as buckets.

    TODO use FNV-1a hash by default to unsigned keys
 */
struct HashSet(T,
               alias Allocator = null,
               alias hashFunction = murmurHash3Of!T)
{
    /** Construct with room for storing `capacity` number of elements.
     */
    static typeof(this) withCapacity(size_t capacity)
    {
        typeof(return) that;
        that.initialize(capacity / smallBucketLength);
        return that;
    }

    /** Initialize at least `requestedBucketCount` number of initial buckets.
     */
    private void initialize(size_t requestedBucketCount)
    {
        import std.math : nextPow2;
        immutable bucketCount = nextPow2(requestedBucketCount == 0 ? 0 : requestedBucketCount - 1);
        hashMask = bucketCount - 1;
        initializeBuckets(bucketCount);
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
        foreach (const bucketIndex; 0 .. _buckets.length)
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

    /** Initialize `bucketCount` number of buckets.
     */
    private void initializeBuckets(size_t bucketCount) @trusted // TODO remove @trusted
    {
        _buckets = Buckets.withLength(bucketCount);
        _largeBucketFlags = LargeBucketFlags.withLength(bucketCount);
    }

    /** Insert `value`.
        Returns: `true` if value was already present, `false` otherwise. This is
        similar to behaviour of `contains`.
     */
    bool insert(T value) @trusted
    {
        dln(value);
        import std.algorithm.searching : canFind;
        immutable bucketIndex = bucketHashIndex(value);
        if (_largeBucketFlags[bucketIndex]) // if `_buckets[buckedIndex]` is `Large`
        {
            dln("large");
            if (!_buckets[bucketIndex].large[].canFind(value))
            {
                _buckets[bucketIndex].large.insertBackMove(value);
                return false;
            }
        }
        else                    // otherwise  `_buckets[buckedIndex]` is `Small`
        {
            dln("small: length:",  _buckets[bucketIndex].small.length);
            if (!_buckets[bucketIndex].small[].canFind(value))
            {
                const ok = _buckets[bucketIndex].small.insertBackMaybe(value);
                if (!ok)        // if full
                {
                    // expand small to large
                    SmallBucket smallCopy = _buckets[bucketIndex].small;

                    import std.conv : emplace;
                    emplace!(LargeBucket)(&_buckets[bucketIndex].large, smallCopy[]);

                    _largeBucketFlags[bucketIndex] = true; // bucket is now large
                    dln("becomes large");
                }
                return false;
            }
        }
        return true;
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
        return hashFunction(value) & hashMask; // fast modulo calculation
    }

private:
    import basic_uncopyable_array : Array = UncopyableArray; // TODO change to CopyableArray when
    import basic_bitarray : BitArray;

    alias LargeBucket = Array!(T, Allocator);

    import std.algorithm : max;
    enum smallBucketLength = max(1, // at least one element in small bucket
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
    LargeBucketFlags _largeBucketFlags;
    size_t hashMask;
}

/** Alternative to `core.internal.hash.hashOf`.
 */
private ulong murmurHash3Of(T)(in T value) @trusted
    if (isIntegral!T)
{
    import std.digest : makeDigest;
    import std.digest.murmurhash : MurmurHash3;

    auto dig = makeDigest!(MurmurHash3!(128));
    dig.put((cast(const(ubyte)*)(&value))[0 .. value.sizeof]);
    dig.finish();

    const elements = dig.get();
    return elements[0] ^ elements[1];
}

pragma(inline, true)
private size_t identityHashOf(U)(in U value)
    if (isUnsigned!U &&
        U.sizeof < size_t.sizeof)
{
    return value;
}

version = show;


@safe pure nothrow unittest
{
    const elementCount = 2^^10;

    alias T = uint;

    auto s = HashSet!(T, null, /*identityHashOf*/).withCapacity(elementCount);

    foreach (const i; 0 .. elementCount)
    {
        assert(!s.insert(i));   // all new
    }

    foreach (const i; 0 .. elementCount)
    {
        assert(s.insert(i));    // already exist
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
