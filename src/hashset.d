module hashset;

import std.traits : isIntegral, isUnsigned;
import core.internal.hash : hashOf;

/** Hash set storing elements of type `T`.
 */
struct HashSet(T,
               alias Allocator = null,
               alias hashFunction = murmurHash3Of!T)
{
    import basic_uncopyable_array : Array = UncopyableArray; // TODO change to CopyableArray when
    import basic_bitarray : BitArray;

    /** Construct with prepare storage for `elementCount` number of elements.
     */
    pragma(inline, true)
    this(size_t elementCount)
    {
        const requestedBucketCount = 2 * elementCount / smallBucketLength;
        initialize(requestedBucketCount);
    }

    /** Initialize at least `requestedBucketCount` number of initial buckets.
     */
    pragma(inline)
    private void initialize(size_t requestedBucketCount)
    {
        import std.math : nextPow2;
        immutable bucketCount = nextPow2(requestedBucketCount == 0 ? 0 : requestedBucketCount - 1);
        hashMask = bucketCount - 1;
        initializeBuckets(bucketCount);
    }

    /** Initialize `bucketCount` number of buckets.
     */
    pragma(inline, true)
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
        import std.algorithm.searching : canFind;
        immutable bucketIndex = bucketHashIndex(value);
        if (_largeBucketFlags[bucketIndex]) // if `_buckets[buckedIndex]` is `Large`
        {
            if (!_buckets[bucketIndex].large[].canFind(value))
            {
                _buckets[bucketIndex].large.insertBackMove(value);
                return false;
            }
        }
        else                    // otherwise  `_buckets[buckedIndex]` is `Small`
        {
            if (!_buckets[bucketIndex].small[].canFind(value))
            {
                const ok = _buckets[bucketIndex].small.insertBackMaybe(value);
                if (!ok)        // if full
                {
                    // expand small to large
                    SmallBucket smallCopy = _buckets[bucketIndex].small;
                    _buckets[bucketIndex].large = LargeBucket(smallCopy[]);
                }
                _largeBucketFlags[bucketIndex] = true; // bucket is now large
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
    }

    /** Get index into `bucket` for `value`.
     */
    pragma(inline, true)
    size_t bucketHashIndex(in T value) const
    {
        return hashFunction(value) & hashMask; // fast modulo calculation
    }

private:
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

/** Alternative to `core.internal.hash.hashOf`. */
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

    auto s = HashSet!(T, null, /*identityHashOf*/)(elementCount);

    foreach (const i; 0 .. elementCount)
    {
        assert(!s.insert(i));   // all new
    }

    foreach (const i; 0 .. elementCount)
    {
        assert(s.insert(i));    // already exist
    }

    // size_t usedBucketCount = 0;
    // foreach (const bucketIndex; 0 .. s._buckets.length)
    // {
    //     const length = s._buckets[bucketIndex].length;
    //     if (length != 0)
    //     {
    //         // dln("bucket[", bucketIndex, "].length:", length);
    //         usedBucketCount += 1;
    //     }
    // }

    // version(show)
    // {
    //     dln("Element count: ", elementCount);
    //     dln("Bucket usage: ", usedBucketCount, "/", s._buckets.length);
    // }

    // assert(usedBucketCount == 405);
}

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio : dln;
}
