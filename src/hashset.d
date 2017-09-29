module hashset;

import core.internal.hash : hashOf;

/** Hash set storing elements of type `T`.

    TODO add union storage for small arrays together with smallArrayFlags BitArray
 */
struct HashSet(T,
               alias Allocator = null,
               alias hashFunction = hashOf)
{
    import basic_uncopyable_array : Array = UncopyableArray; // TODO change to CopyableArray when

    /** Construct with at least `requestedBucketCount` number of initial buckets.
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

    /** Remove `value`.
        Returns: `true` if values was removed, `false` otherwise.
     */
    bool remove(U)(in U value)
        if (is(typeof(T.init == U.init)))
    {
        import std.algorithm.searching : find;

        const index = hashFunction(value) & hashMask;

        const bucketSlice = _buckets[index][];
        const hit = bucketSlice.find(value);
        if (hit)
        {
            const offset = hit.ptr - bucketSlice.ptr;
            static assert(0, "TODO Implement popAtIndex in Array and use here");
            if (hit)
            {
                return true;
            }
        }
        return false;
    }

private:
    alias Bucket = Array!(T, Allocator);
    alias Buckets = Array!(Bucket, Allocator);

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
