/** Array-only overloads of Phobos algorithms.
 *
 * Provides more than twice as fast compilation for `char`-arrays (`string`s).
 *
 * See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
 * See_Also: https://forum.dlang.org/thread/ybamybeakxwxwleebnwb@forum.dlang.org?page=1
 */
module array_algorithm;

/** Array-overload for `startsWith` with default predicate.
 *
 * See_Also: https://d.godbolt.org/z/ejEmrK
 */
bool startsWith(T)(scope const T[] haystack,
                   scope const T[] needle)
{
    if (haystack.length >= needle.length)
    {
        return haystack[0 .. needle.length] == needle;
    }
    return false;
}
/// ditto
bool startsWith(T)(scope const T[] haystack,
                   scope const T needle) // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
{
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
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
bool endsWith(T)(scope const T[] haystack,
                 scope const T[] needle)
{
    if (haystack.length >= needle.length)
    {
        return haystack[$ - needle.length .. $] == needle;
    }
    return false;
}
/// ditto
bool endsWith(T)(scope const T[] haystack,
                 scope const T needle)
{
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
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
                 scope const T[] needle)
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
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
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
                     scope const T[] needle)
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
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
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

/** Array-overload for `stripLeft` with default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
inout(T)[] stripLeft(T)(scope return inout(T)[] haystack,
                        scope const T needle)
{
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
    size_t offset = 0;
    while (offset != haystack.length &&
           haystack[offset] == needle) // TODO elide range-check
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
    assert(" _ beta _ ".stripLeft(' ') == "_ beta _ ");
    assert(" _  beta _ ".stripLeft(' ') == "_  beta _ ");
}

/** Array-overload for `stripRight` with default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
inout(T)[] stripRight(T)(scope return inout(T)[] haystack,
                         scope const T needle)
{
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
    size_t offset = haystack.length;
    while (offset != 0 &&
           haystack[offset - 1] == needle) // TODO elide range-check
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
    assert(" _ beta _ ".stripRight(' ') == " _ beta _");
    assert(" _  beta _ ".stripRight(' ') == " _  beta _");
}

/** Array-overload for `strip` with default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
inout(T)[] strip(T)(scope return inout(T)[] haystack,
                    scope const T needle)
{
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org

    size_t leftOffset = 0;
    while (leftOffset != haystack.length &&
           haystack[leftOffset] == needle) // TODO elide range-check
    {
        leftOffset += 1;
    }

    size_t rightOffset = haystack.length;
    while (rightOffset != leftOffset &&
           haystack[rightOffset - 1] == needle) // TODO elide range-check
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
    assert(" _ beta _ ".strip(' ') == "_ beta _");
    assert(" _  beta _ ".strip(' ') == "_  beta _");
}

///
@safe pure nothrow @nogc unittest
{
    const ubyte[3] x = [0, 42, 0];
    assert(x.strip(0) == x[1 .. 2]);
}

/** Array-overload for `count` with default predicate.
 */
size_t count(T)(scope const T[] haystack,
                scope const T[] needle)
{
    assert(needle.length != 0, "Cannot count occurrences of an empty range");
    size_t result = 0;
    if (haystack.length < needle.length)
    {
        return false;
    }
    foreach (const size_t offset; 0 .. haystack.length - needle.length + 1)
    {
        result += haystack[offset .. offset + needle.length] == needle ? 1 : 0;
    }
    return result;
}

///
@safe pure nothrow @nogc unittest
{
    // import std.algorithm.searching : count;
    assert("".count("_") == 0);
    // assert("".count("") == 0);
    assert("".count(" ") == 0);
    assert(" ".count(" ") == 1);
    assert("abc_abc".count("a") == 2);
    assert("_a_a_".count("_") == 3);
    // assert("_a_a_".count("") == 5);
}

/** Array-overload for `count` with default predicate.
 */
size_t count(T)(scope const T[] haystack,
                scope const T needle)
{
    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org
    size_t result;
    foreach (const ref e; haystack)
    {
        result += e == needle ? 1 : 0;
    }
    return result;
}

///
@safe pure nothrow @nogc unittest
{
    assert("".count('_') == 0);
    assert("abc_abc".count('a') == 2);
    assert("_abc_abc_".count('_') == 3);
}

/** Array-overload for `count` with default predicate and no needle.
 */
size_t count(T)(scope const T[] haystack)
{
    return haystack.length;
}

///
@safe pure nothrow @nogc unittest
{
    assert("abc_abc".count == 7);
}

/** Array-overload for `findSplit` with default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
auto findSplit(T)(scope inout(T)[] haystack,
                  scope const T needle)
{
    static struct Result
    {
        private T[] _haystack;
        private size_t _offset;

        inout(T)[] pre() @trusted inout
        {
            return _haystack.ptr[0 .. _offset];
        }

        inout(T)[] separator() @trusted inout
        {
            if (_isMiss) { return _haystack[$ .. $]; }
            return _haystack.ptr[_offset .. _offset + 1];
        }

        inout(T)[] post() @trusted inout
        {
            if (_isMiss) { return _haystack[$ .. $]; }
            return _haystack.ptr[_offset + 1 .. _haystack.length];
        }

        bool opCast(T : bool)() const
        {
            return !_isMiss;
        }

        private bool _isMiss() const
        {
            return _haystack.length == _offset;
        }
    }

    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org

    foreach (const offset, const ref e; haystack)
    {
        if (e == needle)
        {
            return inout(Result)(haystack, offset);
        }
    }

    return inout(Result)(haystack, haystack.length);
}

///
@safe pure nothrow @nogc unittest
{
    const r = "a*b".findSplit('*');
    assert(r);
    assert(r.pre == "a");
    assert(r.separator == "*");
    assert(r.post == "b");
}

/** Array-overload for `findSplitBefore` with default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
auto findSplitBefore(T)(scope inout(T)[] haystack,
                        scope const T needle)
{
    static struct Result
    {
        private T[] _haystack;
        private size_t _offset;

    pragma(inline, true):

        inout(T)[] pre() @trusted inout
        {
            return _haystack.ptr[0 .. _offset];
        }

        inout(T)[] post() @trusted inout
        {
            if (_isMiss) { return _haystack[$ .. $]; }
            return _haystack.ptr[_offset .. _haystack.length];
        }

        bool opCast(T : bool)() const
        {
            return !_isMiss;
        }

        private bool _isMiss() const
        {
            return _haystack.length == _offset;
        }
    }

    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org

    foreach (const offset, const ref e; haystack)
    {
        if (e == needle)
        {
            return inout(Result)(haystack, offset);
        }
    }

    return inout(Result)(haystack, haystack.length);
}

///
@safe pure nothrow @nogc unittest
{
    const r = "a*b".findSplitBefore('*');
    assert(r);
    assert(r.pre == "a");
    assert(r.post == "*b");
}

///
@safe pure nothrow @nogc unittest
{
    const r = "a*b".findSplitBefore('_');
    assert(!r);
    assert(r.pre == "a*b");
    assert(r.post == "");
}

/** Array-overload for `findSplitAfter` with default predicate.
 *
 * See_Also: https://forum.dlang.org/post/dhxwgtaubzbmjaqjmnmq@forum.dlang.org
 */
auto findSplitAfter(T)(scope inout(T)[] haystack,
                       scope const T needle) @trusted
{
    static struct Result
    {
        private T[] _haystack;
        private size_t _offset;

    pragma(inline, true):

        inout(T)[] pre() @trusted inout
        {
            if (_isMiss) { return _haystack[$ .. $]; }
            return _haystack.ptr[0 .. _offset + 1];
        }

        inout(T)[] post() @trusted inout
        {
            if (_isMiss) { return _haystack[0 .. $]; }
            return _haystack.ptr[_offset + 1 .. _haystack.length];
        }

        bool opCast(T : bool)() const
        {
            return !_isMiss;
        }

        private bool _isMiss() const
        {
            return _haystack.length == _offset;
        }
    }

    static if (is(T == char)) { assert(needle < 128); } // See_Also: https://forum.dlang.org/post/sjirukypxmmcgdmqbcpe@forum.dlang.org

    foreach (const offset, const ref e; haystack)
    {
        if (e == needle)
        {
            return inout(Result)(haystack, offset);
        }
    }

    return inout(Result)(haystack, haystack.length);
}

///
@safe pure nothrow @nogc unittest
{
    char[] haystack;
    auto r = haystack.findSplitAfter('*');
    static assert(is(typeof(r.pre()) == char[]));
    static assert(is(typeof(r.post()) == char[]));
}

///
@safe pure nothrow @nogc unittest
{
    const(char)[] haystack;
    auto r = haystack.findSplitAfter('*');
    static assert(is(typeof(r.pre()) == const(char)[]));
    static assert(is(typeof(r.post()) == const(char)[]));
}

///
@safe pure nothrow @nogc unittest
{
    auto r = "a*b".findSplitAfter('*');
    static assert(is(typeof(r.pre()) == string));
    static assert(is(typeof(r.post()) == string));
    assert(r);
    assert(r.pre == "a*");
    assert(r.post == "b");
}
