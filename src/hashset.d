module hashset;

struct HashSet(T,
               alias Allocator = null) // null means means to qcmeman functions
{
    import basic_uncopyable_array : Array = UncopyableArray;

    /** Construct with at least `requestedMinimumBucketCount` number of initial
        buckets.
     */
    this(size_t requestedMinimumBucketCount)
    {
        import std.math : nextPow2;
        _buckets = Buckets.withLength(nextPow2(requestedMinimumBucketCount));
    }

    void insert(T value)
    {
        import core.internal.hash : hashOf;
        const hash = hashOf(value);
    }

private:
    alias Bucket = Array!(T);
    alias Buckets = Array!Bucket;
    Buckets _buckets;
}

@safe pure nothrow @nogc unittest
{
    alias T = uint;
    auto s = HashSet!T(15);
    assert(s._buckets.length == 16);
}

version(unittest)
{
    import array_help : s;
}
