/** Array-only overloads of Phobos algorithms.
 */
module array_algorithm;

import std.range.primitives : ElementType;

/** Array-overload for `startsWith` with no explicit predicate predicate. */
bool startsWith(T)(scope const(T)[] haystack,
                   scope const(T)[] needle)
{
    if (haystack.length >= needle.length)
    {
        return haystack[0 .. needle.length] == needle; // range check is elided by LDC in release builds
    }
    return false;
}
/// ditto
bool startsWith(T)(scope const(T)[] haystack,
                   scope T needle)
{
    if (haystack.length >= 1)
    {
        return haystack[0 .. 1] == needle; // range check is elided by LDC in release builds
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    auto x = "beta version";
    assert(x.startsWith("beta"));
    assert(!x.startsWith("_"));
}

/** Array-overload for `endsWith` with no explicit predicate predicate. */
bool endsWith(T)(scope const(T)[] haystack,
                 scope const(T)[] needle)
{
    if (haystack.length >= needle.length)
    {
        return haystack[$ - needle.length .. $] == needle; // range check is elided by LDC in release builds
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    auto x = "beta version";
    assert(x.endsWith("version"));
    assert(!x.startsWith("_"));
}

/** Array-overload for `skipOver` with no explicit predicate predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
bool skipOver(T)(scope ref inout(T)[] haystack,
                 scope const(T)[] needle)
{
    if (startsWith(haystack, needle))
    {
        haystack = haystack[needle.length .. $];
        return true;
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    string x = "beta version";
    assert(x.skipOver("beta"));
}

/** Array-overload for `skipOverBack` with no explicit predicate predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
bool skipOverBack(T)(scope ref inout(T)[] haystack,
                     scope const(T)[] needle)
{
    if (endsWith(haystack, needle))
    {
        haystack = haystack[0 .. $ - needle.length];
        return true;
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    string x = "beta version";
    assert(x.skipOverBack(" version"));
    assert(x == "beta");
}

/** Overload of `std.array.array` that creates a static array of length `n`.
 *
 * TODO Better name: {make,array}{N,Exactly}
 * TODO could we find a way to propagate length at compile-time?
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
 *
 * See_Also: http://forum.dlang.org/thread/rqlittlysttwxwphlnmh@forum.dlang.org
 * TODO Add to Phobos
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
