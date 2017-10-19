module array_algorithm;

import std.range : ElementType;

/** Overload of `std.array.array` that creates a static array of length `n`.
    TODO Better name: {make,array}{N,Exactly}
    TODO could we find a way to propagate length at compile-time?
 */
ElementType!R[n] toStaticArray(size_t n, R)(R r)
{
    assert(r.length == n);
    typeof(return) dst;
    import std.algorithm.mutation : copy;
    r.copy(dst[]);
    return dst;
}

/** Static array overload for `std.algorithm.iteration.map`.
    See also: http://forum.dlang.org/thread/rqlittlysttwxwphlnmh@forum.dlang.org
    TODO Add to Phobos
 */
typeof(fun(E.init))[n] map(alias fun, E, size_t n)(const E[n] src)
{
    import std.algorithm.iteration : map;
    return src[].map!fun.toStaticArray!n;
}

///
@safe pure nothrow unittest
{
    import std.meta : AliasSeq;
    foreach (E; AliasSeq!(int, double))
    {
        enum n = 42;
        E[n] c;
        const result = c.map!(_ => _^^2);
        static assert(c.length == result.length);
        static assert(is(typeof(result) == const(E)[n]));
    }
}
