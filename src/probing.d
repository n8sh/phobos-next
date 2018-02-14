/** Probing algoriths used by for instance hash tables.
 */
module probing;

/** Search for `key` in `haystack` starting at `index` in steps of triangular numbers.
 *
 * Returns: index into `haystack` upon hit, `haystack.length` upon miss.
 * Note: `haystack.length` must be a power of two.
 * See also: https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
 */
size_t triangularProbeIndexFrom(alias predicate, T)(const scope T[] haystack, size_t index)
{
    import std.functional : unaryFun;

    immutable typeof(return) mask = haystack.length - 1;
    assert((~mask ^ mask) == typeof(return).max); // std.math.isPowerOf2(haystack.length)

    // search using triangular numbers as increments
    size_t indexIncrement = 0;
    while (indexIncrement != haystack.length)
    {
        if (unaryFun!predicate(haystack[index]))
        {
            return index;
        }
        indexIncrement += 1;
        index = (index + indexIncrement) & mask;
    }

    return haystack.length;
}

///
@safe pure nothrow unittest
{
    import std.typecons : Nullable;

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

        assert(haystack.triangularProbeIndexFrom!(element => (element is hitKey ||
                                                              element.isNull))(lengthPower) != haystack.length);
        assert(haystack.triangularProbeIndexFrom!(element => (element is missKey ||
                                                              element.isNull))(lengthPower) == haystack.length);
    }
}

version(unittest)
{
    import dbgio;
}
