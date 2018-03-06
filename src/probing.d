/** Probing algoriths used by for instance hash tables.
 */
module probing;

import std.functional : unaryFun, binaryFun;

/** Search for a key in `haystack` matching `predicate` starting at `index` in
 * steps of triangular numbers, 0,1,3,6,10,15,21, ... .
 *
 * If `assumeNotFull` is `true` it is assumed that at least one element in
 * `haystack` matches `predicate`, thereby enabling sentinel-based probing. Such
 * probing doesn't require in-loop range checking via `indexIncrement !=
 * haystack.length` and can be made faster.
 *
 * Returns: index into `haystack` upon hit, `haystack.length` upon miss.
 * Note: `haystack.length` must be a power of two (or 1 or zero).
 * See also: https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
 */
size_t triangularProbeFromIndex(alias predicate,
                                alias assumeNotFull = false,
                                T)(const scope T[] haystack, size_t index)
    if (is(typeof(unaryFun!predicate(T.init))) ||
        is(typeof(binaryFun!predicate(size_t.init, T.init))))
{
    immutable mask = haystack.length - 1;
    assert((~mask ^ mask) == typeof(return).max); // std.math.isPowerOf2(haystack.length)

    // search using triangular numbers as increments
    size_t indexIncrement = 0;
    while (true)
    {
        static if (assumeNotFull)
        {
            assert(indexIncrement != haystack.length,
                   "no element `haystack` matches `predicate`");
        }
        else
        {
            if (indexIncrement == haystack.length)
            {
                return haystack.length;
            }
        }

        static if (is(typeof(unaryFun!predicate(T.init))))
        {
            if (unaryFun!predicate(haystack[index]))
            {
                return index;
            }
        }
        else static if (is(typeof(binaryFun!predicate(size_t.min, T.init))))
        {
            if (binaryFun!predicate(index, haystack[index]))
            {
                return index;
            }
        }
        else
        {
            static assert(0, "unsupported predicate");
        }
        indexIncrement += 1;
        index = (index + indexIncrement) & mask; // next triangular number modulo length
    }
}

/// empty case
@safe pure nothrow unittest
{
    alias T = Nullable!int;

    immutable length = 0;
    immutable hitKey = T(42); // key to store
    auto haystack = new T[length];

    assert(haystack.triangularProbeFromIndex!((T element) => (element is hitKey ||
                                                              element.isNull))(0) == haystack.length);
    assert(haystack.triangularProbeFromIndex!((size_t index, T element) => true)(0) == 0);
    assert(haystack.triangularProbeFromIndex!((size_t index, T element) => false)(0) == haystack.length);
}

/// generic case
@safe pure nothrow unittest
{
    alias T = Nullable!int;

    foreach (immutable lengthPower; 0 .. 20)
    {
        immutable length = 2^^lengthPower;

        immutable hitKey = T(42);  // key to store
        immutable missKey = T(43); // other key not present

        auto haystack = new T[length];
        haystack[] = T(17);     // make haystack full
        haystack[$/2] = hitKey;

        alias elementHitPredicate = element => (element is hitKey || element.isNull);
        alias elementMissPredicate = element => (element is missKey || element.isNull);

        // key hit
        assert(haystack.triangularProbeFromIndex!(elementHitPredicate)(lengthPower) != haystack.length);

        // key miss
        assert(haystack.triangularProbeFromIndex!(elementMissPredicate)(lengthPower) == haystack.length);
    }
}

version(unittest)
{
    import std.typecons : Nullable;
}

@trusted pure unittest
{
    class C { int value; }
    C x;
    C y = cast(C)((cast(size_t*)null) + 1); // indicates a lazily deleted key
    struct S
    {
        // TODO make these work:
        // enum C hole1 = cast(C)((cast(size_t*)null) + 1);
        // static immutable C hole2 = cast(C)((cast(size_t*)null) + 1);
    }
}
