module hashset;

struct HashSet(T,
               alias Allocator = null) // null means means to qcmeman functions
{
    import basic_uncopyable_array : Array = UncopyableArray;

    /// Construct with `bucketCount` number of initial buckets.
    this(size_t bucketCount)
    {
        _buckets = Buckets.withLength(bucketCount);
    }
private:
    alias Bucket = Array!(T);
    alias Buckets = Array!Bucket;
    Buckets _buckets;
}

@safe pure nothrow @nogc unittest
{
    alias T = uint;
    HashSet!T s;
}

version(unittest)
{
    import array_help : s;
}
