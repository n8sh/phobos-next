module hashset;

import std.traits : isIntegral, isUnsigned;
import core.internal.hash : hashOf;

/** Hash set storing elements of type `T`.

    TODO add union storage for small arrays together with smallArrayFlags BitArray
 */
struct HashSet(T,
               alias Allocator = null,
               alias hashFunction = murmurHash3Of!T)
{
    import basic_uncopyable_array : Array = UncopyableArray; // TODO change to CopyableArray when

    /** Construct with at least `requestedBucketCount` number of initial buckets.
     */
    pragma(inline, true)
    this(size_t requestedBucketCount)
    {
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
    }

    /** Insert `value`.
        Returns: `true` if value was already present, `false` otherwise. This is
        similar to behaviour of `contains`.
     */
    bool insert(T value)
    {
        import std.algorithm.searching : canFind;
        immutable bucketIndex = bucketHashIndex(value);
        // dln(bucketIndex);
        if (!_buckets[bucketIndex][].canFind(value))
        {
            _buckets[bucketIndex].insertBackMove(value);
            return false;
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
    alias Bucket = Array!(T, Allocator);
    alias Buckets = Array!(Bucket, Allocator);

    Buckets _buckets;
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

// version = show;

@safe pure nothrow unittest
{
    const bucketCount = 2^^10;
    const elementCount = bucketCount/2;

    alias T = uint;

    auto s = HashSet!(T, null, /*identityHashOf*/)(bucketCount);

    assert(s._buckets.length == bucketCount);

    foreach (const i; 0 .. elementCount)
    {
        assert(!s.insert(i));   // all new
    }

    foreach (const i; 0 .. elementCount)
    {
        assert(s.insert(i));    // already exist
    }

    size_t usedBucketCount = 0;
    foreach (const bucketIndex; 0 .. bucketCount)
    {
        const length = s._buckets[bucketIndex].length;
        if (length != 0)
        {
            // dln("bucket[", bucketIndex, "].length:", length);
            usedBucketCount += 1;
        }
    }

    version(show)
    {
        dln("Element count: ", elementCount);
        dln("Bucket usage: ", usedBucketCount, "/", bucketCount);
    }

    assert(usedBucketCount == 405);
}

version(unittest)
{
    import array_help : s;
}

version(show)
{
    import dbgio : dln;
}
