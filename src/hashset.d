module hashset;

struct HashSet(T,
               alias Allocator = null, // null means means to qcmeman functions
               alias hashFunction = null)
{
    import core.internal.hash : hashOf;
    import basic_uncopyable_array : Array = UncopyableArray;

    /** Construct with at least `requestedMinimumBucketCount` number of initial
        buckets.
     */
    this(size_t requestedMinimumBucketCount)
    {
        import std.math : nextPow2;
        immutable bucketCount = nextPow2(requestedMinimumBucketCount);
        hashMask = bucketCount - 1;
        _buckets = Buckets.withLength(bucketCount);
    }

    /** Insert `value`. */
    void insert(T value)
    {
        const hash = hashOf(value);
        const size_t index = hash & hashMask;
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
        s.insert(i);
    }
}

version(unittest)
{
    import array_help : s;
    import dbgio : dln;
}
