/** Array-only overloads of Phobos algorithms.
 *
 * Provides more than twice as fast compilation for `char`-arrays (`string`s).
 */
module array_algorithm;

import std.range.primitives : ElementType;

/** Array-overload for `startsWith` with default predicate. */
bool startsWith(T)(scope const(T)[] haystack,
                   scope const(T)[] needle)
{
    if (haystack.length >= needle.length)
    {
        return haystack[0 .. needle.length] == needle;
    }
    return false;
}
/// ditto
bool startsWith(T)(scope const(T)[] haystack,
                   scope const T needle)
{
    static if (is(T : char)) { assert(needle < 128); } // TODO convert needle to string and call itself
    if (haystack.length >= 1)
    {
        return haystack[0] == needle;
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    auto x = "beta version";
    assert(x.startsWith("beta"));
    assert(x.startsWith('b'));
    assert(!x.startsWith("_"));
}

/** Array-overload for `endsWith` with default predicate. */
bool endsWith(T)(scope const(T)[] haystack,
                 scope const(T)[] needle)
{
    if (haystack.length >= needle.length)
    {
        return haystack[$ - needle.length .. $] == needle;
    }
    return false;
}
/// ditto
bool endsWith(T)(scope const(T)[] haystack,
                 scope const T needle)
{
    static if (is(T : char)) { assert(needle < 128); } // TODO convert needle to string and call itself
    if (haystack.length >= 1)
    {
        return haystack[$ - 1] == needle;
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    auto x = "beta version";
    assert(x.endsWith("version"));
    assert(x.endsWith('n'));
    assert(!x.startsWith("_"));
}

/** Array-overload for `skipOver` with default predicate.
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
/// ditto
bool skipOver(T)(scope ref inout(T)[] haystack,
                 scope const T needle)
{
    static if (is(T : char)) { assert(needle < 128); } // TODO convert needle to string and call itself
    if (startsWith(haystack, needle))
    {
        haystack = haystack[1 .. $];
        return true;
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    string x = "beta version";
    assert(x.skipOver("beta"));
    assert(x == " version");
    assert(x.skipOver(' '));
    assert(x == "version");
}

/** Array-overload for `skipOverBack` with default predicate.
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
/// ditto
bool skipOverBack(T)(scope ref inout(T)[] haystack,
                     scope const T needle)
{
    static if (is(T : char)) { assert(needle < 128); } // TODO convert needle to string and call itself
    if (endsWith(haystack, needle))
    {
        haystack = haystack[0 .. $ - 1];
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
    assert(x.skipOverBack('a'));
    assert(x == "bet");
}

/** Array-overload for `stripLeft` with ASCII-char needle and default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
inout(T)[] stripLeft(T)(scope return inout(T)[] haystack,
                        scope const T needle)
{
    static if (is(T : char)) { assert(needle < 128); } // TODO convert needle to string and call itself
    size_t offset = 0;
    while (offset != haystack.length &&
           haystack[offset] == needle)
    {
        offset += 1;
    }
    return haystack[offset .. $];
}
inout(char)[] stripLeft()(scope return inout(char)[] haystack) @safe pure nothrow @nogc // template-lazy
{
    return haystack.stripLeft(' ');
}

///
@safe pure nothrow @nogc unittest
{
    assert("beta".stripLeft(' ') == "beta");
    assert(" beta".stripLeft(' ') == "beta");
    assert("  beta".stripLeft(' ') == "beta");
    assert("   beta".stripLeft(' ') == "beta");
    assert("   beta".stripLeft() == "beta");
}

/** Array-overload for `stripRight` with ASCII-char needle and default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
inout(T)[] stripRight(T)(scope return inout(T)[] haystack,
                         scope const T needle)
{
    static if (is(T : char)) { assert(needle < 128); } // TODO convert needle to string and call itself
    size_t offset = haystack.length;
    while (offset != 0 &&
           haystack[offset - 1] == needle)
    {
        offset -= 1;
    }
    return haystack[0 .. offset];
}
inout(char)[] stripRight()(scope return inout(char)[] haystack) @safe pure nothrow @nogc // template-lazy
{
    return haystack.stripRight(' ');
}

///
@safe pure nothrow @nogc unittest
{
    assert("beta".stripRight(' ') == "beta");
    assert("beta ".stripRight(' ') == "beta");
    assert("beta  ".stripRight(' ') == "beta");
    assert("beta    ".stripRight(' ') == "beta");
    assert("beta    ".stripRight() == "beta");
}

/** Array-overload for `strip` with ASCII-char needle and default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
inout(T)[] strip(T)(scope return inout(T)[] haystack,
                    scope const T needle)
{
    static if (is(T : char)) { assert(needle < 128); } // TODO convert needle to string and call itself

    size_t leftOffset = 0;
    while (leftOffset != haystack.length &&
           haystack[leftOffset] == needle)
    {
        leftOffset += 1;
    }

    size_t rightOffset = haystack.length;
    while (rightOffset != leftOffset &&
           haystack[rightOffset - 1] == needle)
    {
        rightOffset -= 1;
    }

    return haystack[leftOffset .. rightOffset];
}
inout(char)[] strip()(scope return inout(char)[] haystack) @safe pure nothrow @nogc // template-lazy
{
    return haystack.strip(' ');
}

///
@safe pure nothrow @nogc unittest
{
    assert("beta".strip(' ') == "beta");
    assert(" beta ".strip(' ') == "beta");
    assert("  beta  ".strip(' ') == "beta");
    assert("   beta   ".strip(' ') == "beta");
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
