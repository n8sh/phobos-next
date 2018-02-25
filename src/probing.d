/** Probing algoriths used by for instance hash tables.
 */
module probing;

import std.functional : unaryFun;

/** Search for a key in `haystack` matching `elementPredicate` starting at `index` in
 * steps of triangular numbers, 0,1,3,6,10,15,21, ... .
 *
 * Returns: index into `haystack` upon hit, `haystack.length` upon miss.
 * Note: `haystack.length` must be a power of two (or 1 or zero).
 * See also: https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
 */
size_t triangularProbeFromIndex(alias elementPredicate, alias indexPredicate, T)(const scope T[] haystack, size_t index)
    if (is(typeof(unaryFun!elementPredicate(T.init))))
{
    immutable typeof(return) mask = haystack.length - 1;
    assert((~mask ^ mask) == typeof(return).max); // std.math.isPowerOf2(haystack.length)

    // search using triangular numbers as increments
    size_t indexIncrement = 0;
    while (indexIncrement != haystack.length)
    {
        if (unaryFun!elementPredicate(haystack[index]))
        {
            return index;
        }
        indexIncrement += 1;
        index = (index + indexIncrement) & mask;
    }

    return haystack.length;
}

/// empty case
@safe pure nothrow unittest
{
    alias T = Nullable!int;

    immutable mask = size_t.min - 1;
    assert((~mask ^ mask) == typeof(mask).max); // std.math.isPowerOf2(haystack.length)

    immutable length = 0;
    immutable hitKey = T(42); // key to store
    auto haystack = new T[length];

    // any key misses
    alias elementPredicate = _ => (_ is hitKey || _.isNull);
    enum indexPredicate = null;
    assert(haystack.triangularProbeFromIndex!(elementPredicate, indexPredicate)(0) == haystack.length);
}

/// generic case
@safe pure nothrow unittest
{
    alias T = Nullable!int;

    foreach (immutable lengthPower; 0 .. 20)
    {
        immutable length = 2^^lengthPower;

        immutable hitKey = T(42); // key to store
        immutable missKey = T(43); // other key not present

        auto haystack = new T[length];
        haystack[] = T(17);     // make haystack full
        haystack[$/2] = hitKey;

        alias elementHitPredicate = _ => (_ is hitKey || _.isNull);
        alias elementMissPredicate = _ => (_ is missKey || _.isNull);
        enum indexPredicate = null;

        // key hit
        assert(haystack.triangularProbeFromIndex!(elementHitPredicate, indexPredicate)(lengthPower) != haystack.length);

        // key miss
        assert(haystack.triangularProbeFromIndex!(elementMissPredicate, indexPredicate)(lengthPower) == haystack.length);
    }
}

version(unittest)
{
    import std.typecons : Nullable;
}
