module find_split_ex;

import std.traits : isExpressions;

/** Like `findSplit` but with multiple separator `needles` known at compile-time
 * to prevent `NarrowString` decoding.
 *
 * TODO Do sentinel-based search when `haystack` is mutable and larger than a
 * certain value.
 *
 * TODO Add to Phobos.
 *
 * TODO Resort to `memchr` for some case `if (!__ctfe)`.
 * See_Also: https://forum.dlang.org/post/efpbmtyisamwwqgpxnbq@forum.dlang.org
 *
 * See_Also: https://forum.dlang.org/post/ycotlbfsqoupogaplkvf@forum.dlang.org
 */
template findSplitAmong(needles...)
if (needles.length != 0 &&
    isExpressions!needles)
{
    import std.meta : allSatisfy;
    import char_traits : isASCII;

    auto findSplitAmong(Haystack)(const scope return Haystack haystack) @trusted // TODO qualify with `inout` to reduce template bloat
    if (is(typeof(Haystack.init[0 .. 0])) && // can be sliced
        is(typeof(Haystack.init[0]) : char) &&
        allSatisfy!(isASCII, needles))
    {
        // similar return result to `std.algorithm.searching.findSplit`
        static struct Result
        {
            /* Only requires 3 words opposite to Phobos' `findSplit`,
             * `findSplitBefore` and `findSplitAfter`:
             */

            private Haystack _haystack; // original copy of haystack
            private size_t _offset; // hit offset if any, or `_haystack.length` if miss

            bool opCast(T : bool)() const
            {
                return !empty;
            }

            @property:

            inout(Haystack) pre() inout
            {
                return _haystack[0 .. _offset];
            }

            inout(Haystack) separator() inout
            {
                if (empty) { return _haystack[$ .. $]; }
                return _haystack[_offset .. _offset + 1];
            }

            inout(Haystack) post() inout
            {
                if (empty) { return _haystack[$ .. $]; }
                return _haystack[_offset + 1 .. $];
            }

            inout(Haystack) opIndex()(size_t idx) inout
            {
                switch (idx)
                {
                case 0: return pre;
                case 1: return separator;
                case 2: return post;
                default: assert(0, "Index out of bounds");
                }
            }

            @property private bool empty() const
            {
                return _haystack.length == _offset;
            }
        }

        enum use_memchr = false;
        static if (use_memchr &&
                   needles.length == 1)
        {
            // See_Also: https://forum.dlang.org/post/piowvfbimztbqjvieddj@forum.dlang.org
            import core.stdc.string : memchr;
            // extern (C) @system nothrow @nogc pure void* rawmemchr(return const void* s, int c);

            const void* hit = memchr(haystack.ptr, needles[0], haystack.length);
            return Result(haystack, hit ? hit - cast(const(void)*)haystack.ptr : haystack.length);
        }
        else
        {
            foreach (immutable offset; 0 .. haystack.length)
            {
                static if (needles.length == 1)
                {
                    immutable hit = haystack[offset] == needles[0];
                }
                else
                {
                    import std.algorithm.comparison : among;
                    immutable hit = haystack[offset].among!(needles) != 0;
                }
                if (hit)
                {
                    return Result(haystack, offset);
                }
            }
            return Result(haystack, haystack.length);
        }
    }
}

template findSplit(needles...)
if (needles.length == 1 &&
    isExpressions!needles)
{
    import std.meta : allSatisfy;
    import char_traits : isASCII;

    auto findSplit(Haystack)(const scope return Haystack haystack) @trusted // TODO qualify with `inout` to reduce template bloat
    if (is(typeof(Haystack.init[0 .. 0])) && // can be sliced
        is(typeof(Haystack.init[0]) : char) &&
        isASCII!(needles[0]))
    {
        return findSplitAmong!(needles)(haystack);
    }
}

///
@safe pure nothrow @nogc unittest
{
    const r = "a*b".findSplit!('*');
    assert(r);

    assert(r[0] == "a");
    assert(r.pre == "a");

    assert(r[1] == "*");
    assert(r.separator == "*");

    assert(r[2] == "b");
    assert(r.post == "b");
}

///
@safe pure nothrow @nogc unittest
{
    auto r = "a+b*c".findSplitAmong!('+', '-');

    static assert(r.sizeof == 24);
    static assert(is(typeof(r.pre) == string));
    static assert(is(typeof(r.separator) == string));
    static assert(is(typeof(r.post) == string));

    assert(r);

    assert(r[0] == "a");
    assert(r.pre == "a");

    assert(r[1] == "+");
    assert(r.separator == "+");

    assert(r[2] == "b*c");
    assert(r.post == "b*c");
}

///
@safe pure nothrow @nogc unittest
{
    const r = "a+b*c".findSplitAmong!('-', '*');
    assert(r);
    assert(r.pre == "a+b");
    assert(r.separator == "*");
    assert(r.post == "c");
}

///
@safe pure nothrow @nogc unittest
{
    const r = "a*".findSplitAmong!('*');

    assert(r);

    assert(r[0] == "a");
    assert(r.pre == "a");

    assert(r[1] == "*");
    assert(r.separator == "*");

    assert(r[2] == "");
    assert(r.post == "");
}

///
@safe pure nothrow @nogc unittest
{
    const r = "*b".findSplitAmong!('*');

    assert(r);

    assert(r[0] == "");
    assert(r.pre == "");

    assert(r[1] == "*");
    assert(r.separator == "*");

    assert(r[2] == "b");
    assert(r.post == "b");
}

///
@safe pure nothrow @nogc unittest
{
    const r = "*".findSplitAmong!('*');

    assert(r);

    assert(r[0] == "");
    assert(r.pre == "");

    assert(r[1] == "*");
    assert(r.separator == "*");

    assert(r[2] == "");
    assert(r.post == "");
}

///
@safe pure nothrow @nogc unittest
{
    static immutable separator_char = '/';

    immutable r = "a+b*c".findSplitAmong!(separator_char);

    static assert(r.sizeof == 24);
    static assert(is(typeof(r.pre) == immutable string));
    static assert(is(typeof(r.separator) == immutable string));
    static assert(is(typeof(r.post) == immutable string));

    assert(!r);

    assert(r.pre == "a+b*c");
    assert(r[0] == "a+b*c");
    assert(r.separator == []);
    assert(r[1] == []);
    assert(r.post == []);
    assert(r[2] == []);
}
