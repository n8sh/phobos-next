module hashset;

import core.internal.hash : hashOf;

/** Hash set storing elements of type `T`.

    TODO add union storage for small arrays together with smallArrayFlags BitArray
 */
struct HashSet(T,
               alias Allocator = null, // null means means to qcmeman functions
               alias hashFunction = hashOf)
{
    import basic_uncopyable_array : Array = UncopyableArray; // TODO change to CopyableArray when

    /** Construct with at least `requestedMinimumBucketCount` number of initial
        buckets.
     */
    pragma(inline, true)
    this(size_t requestedBucketCount)
    {
        initialize(requestedBucketCount);
    }

    pragma(inline)
    private void initialize(size_t requestedBucketCount) @safe
    {
        import std.math : nextPow2;
        immutable bucketCount = nextPow2(requestedBucketCount);
        hashMask = bucketCount - 1;
        initializeBuckets(bucketCount);
    }

    pragma(inline, true)
    private void initializeBuckets(size_t bucketCount) @trusted
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
        const index = hashFunction(value) & hashMask;
        if (!_buckets[index][].canFind(value))
        {
            _buckets[index].insertBackMove(value);
            return false;
        }
        return true;
    }

private:
    alias Bucket = Array!(T);
    alias Buckets = Array!Bucket;

    Buckets _buckets;
    size_t hashMask;
}

@safe pure nothrow unittest
{
    const n = 255;
    alias T = uint;
    auto s = HashSet!T(n);

    assert(s._buckets.length == 256);

    foreach (i; 0 .. 16)
    {
        assert(!s.insert(i));
    }

    foreach (i; 0 .. 16)
    {
        assert(s.insert(i));
    }
}

version(unittest)
{
    import array_help : s;
    import dbgio : dln;
}
