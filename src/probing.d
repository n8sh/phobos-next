/** Probing algoriths used by for instance hash tables.
 */
module probing;

/** Search for `key` in `haystack` starting at `index` in steps of triangular numbers.
 *
 * `haystack.length` must be a power of two.
 *
 * See also: https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
 */
size_t triangularProbeIndexFrom(alias hasher = hashOf, T, K)(const scope T[] haystack,
                                                             const scope auto ref K key,
                                                             size_t index)
    @safe pure nothrow @nogc
{
    immutable typeof(return) mask = haystack.length - 1;
    assert((~mask ^ mask) == typeof(return).max); // std.math.isPowerOf2(haystack.length)

    // search using triangular numbers as increments
    size_t indexIncrement = 0;
    while (indexIncrement != haystack.length)
    {
        if (haystack[index] is key || // hit slot
            haystack[index].isNull)   // free slot
        {
            return index;
        }
        indexIncrement += 1;
        index = (index + indexIncrement) & mask;
    }

    return haystack.length;
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
        dln("length:", length, " lengthPower:", lengthPower);

        immutable hitKey = T(42);
        immutable missKey = T(43);

        // allocate and prepare haystack
        auto haystack = new T[length];
        haystack[] = T(17);     // other values are 17
        haystack[$/2] = hitKey;    // set hitKey

        assert(haystack.triangularProbeIndexFrom!(FNV!(64, true))(hitKey, lengthPower) != haystack.length);
        assert(haystack.triangularProbeIndexFrom!(FNV!(64, true))(missKey, lengthPower) == haystack.length);
    }
}

version(unittest)
{
    import dbgio;
}
