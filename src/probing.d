module probing;

/** Quadratic probing of `key` in `haystack` using triangular numbers.
 *
 * haystack.length must be an even power of two.
 *
 * See also: https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
 */
size_t triangularProbeIndexFrom(alias hasher = hashOf, T, K)(const scope T[] haystack,
                                                             const scope auto ref K key,
                                                             size_t index)
    @safe pure nothrow @nogc
{
    size_t isKeyForIndex(const scope K key,
                         const scope size_t index)
    {
        return (haystack[index] is key || // hit slot
                haystack[index].isNull); // free slot
    }

    immutable typeof(return) mask = haystack.length - 1;
    assert((~mask ^ mask) == typeof(return).max); // std.math.isPowerOf2(haystack.length)

    // search using triangular numbers as increments
    size_t indexIncrement = 1;
    while (indexIncrement != haystack.length &&
           !isKeyForIndex(key, index))
    {
        index = (index + indexIncrement) & mask;
        indexIncrement += 1;
    }

    if (isKeyForIndex(key, index))
    {
        return index;
    }
    else
    {
        return haystack.length;
    }
}

@safe pure nothrow unittest
{
    import digestx.fnv : FNV;
    import std.typecons : Nullable;
    import dbgio;

    alias T = Nullable!int;

    foreach (immutable lengthPower; 0 .. 20)
    {
        immutable length = 2^^lengthPower;
        dln(length);

        immutable key = T(42);

        // allocate and prepare haystack
        auto haystack = new T[length];
        haystack[] = T(17);     // other values are 17
        haystack[$-1] = key;    // set key

        import digestion : hashOf2;
        size_t startIx = 0;

        assert(haystack.triangularProbeIndexFrom!(FNV!(64, true))(key, startIx) != haystack.length);
    }
}

version(unittest)
{
    import dbgio;
}
