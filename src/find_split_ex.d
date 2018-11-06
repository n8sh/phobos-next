module find_split_ex;

import std.traits : isExpressions;

/** Like `findSplit` but with multiple separator `needles` known at compile-time
 * to prevent `NarrowString` decoding.
 *
 * TODO Add to Phobos.
 */
template findSplitAmong(needles...)
if (needles.length != 0 &&
    isExpressions!needles)
{
    import std.meta : allSatisfy;
    import traits_ex : isASCII;

    auto findSplitAmong(Haystack)(scope return Haystack haystack)
    if (is(typeof(Haystack.init[0 .. 0])) && // can be sliced
        is(typeof(Haystack.init[0]) : char) &&
        allSatisfy!(isASCII, needles))
    {
        // similar return result to `std.algorithm.searching.findSplit`
        static struct Result
        {
            private Haystack _haystack; // original copy of haystack
            private size_t _offset; // hit offset if any, or `_haystack.length` if miss

            bool opCast(T : bool)() const
            {
                return !_isMatch;
            }

            @property:

            Haystack pre() const
            {
                return _haystack[0 .. _offset];
            }

            Haystack separator() const
            {
                if (_isMatch)
                {
                    return _haystack[$ .. $];
                }
                return _haystack[_offset .. _offset + 1];
            }

            Haystack post() const
            {
                if (_isMatch)
                {
                    return _haystack[$ .. $];
                }
                return _haystack[_offset + 1 .. $];
            }

            private bool _isMatch() const
            {
                return _haystack.length == _offset;
            }
        }

        foreach (immutable offset; 0 .. haystack.length)
        {
            static if (needles.length == 1)
            {
                const bool hit = haystack[offset] == needles[0];
            }
            else
            {
                import std.algorithm.comparison : among;
                const bool hit = haystack[offset].among!(needles) != 0;
            }
            if (hit)
            {
                return Result(haystack, offset);
            }
        }

        return Result(haystack, haystack.length);
    }
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
    assert(r.pre == "a");
    assert(r.separator == "+");
    assert(r.post == "b*c");
}

///
@safe pure nothrow @nogc unittest
{
    const r = "a+b*c".findSplitAmong!('-', '*');
    static assert(r.sizeof == 24);
    static assert(is(typeof(r.pre) == string));
    static assert(is(typeof(r.separator) == string));
    static assert(is(typeof(r.post) == string));
    assert(r);
    assert(r.pre == "a+b");
    assert(r.separator == "*");
    assert(r.post == "c");
}

///
@safe pure nothrow @nogc unittest
{
    immutable r = "a+b*c".findSplitAmong!('/');
    static assert(r.sizeof == 24);
    static assert(is(typeof(r.pre) == string));
    static assert(is(typeof(r.separator) == string));
    static assert(is(typeof(r.post) == string));
    assert(!r);
    assert(r.pre == "a+b*c");
    assert(r.separator == []);
    assert(r.post == []);
}
