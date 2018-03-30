module find_split_ex;

import std.traits : isExpressions;

/** Like `findSplit` but with multiple separator `needles` known at compile-time
 * to prevent `NarrowString` decoding.
 */
template findSplitAmong(needles...)
    if (isExpressions!needles)  // all needs
{
    import std.meta : staticMap, allSatisfy;
    import std.traits : Unqual;
    import traits_ex : allSameTypeIterative, isASCII;

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
                return !_empty;
            }

            @property:

            Haystack pre() const
            {
                return _haystack[0 .. _offset];
            }

            Haystack separator() const
            {
                if (_empty)
                {
                    return _haystack[$ .. $];
                }
                return _haystack[_offset .. _offset + 1];
            }

            Haystack post() const
            {
                if (_empty)
                {
                    return _haystack[$ .. $];
                }
                return _haystack[_offset + 1 .. $];
            }

            private bool _empty() const
            {
                return _haystack.length == _offset;
            }
        }
        foreach (immutable offset; 0 .. haystack.length)
        {
            import std.algorithm.comparison : among;
            if (const uint hitIndex = haystack[offset].among!(needles))
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
    auto r1 = "a+b*c".findSplitAmong!('+', '-');
    static assert(is(typeof(r1.pre) == string));
    static assert(is(typeof(r1.separator) == string));
    static assert(is(typeof(r1.post) == string));
    assert(r1);
    assert(r1.pre == "a");
    assert(r1.separator == "+");
    assert(r1.post == "b*c");

    const r2 = "a+b*c".findSplitAmong!('-', '*');
    static assert(is(typeof(r2.pre) == string));
    static assert(is(typeof(r2.separator) == string));
    static assert(is(typeof(r2.post) == string));
    assert(r2);
    assert(r2.pre == "a+b");
    assert(r2.separator == "*");
    assert(r2.post == "c");

    immutable r3 = "a+b*c".findSplitAmong!('/');
    static assert(is(typeof(r3.pre) == string));
    static assert(is(typeof(r3.separator) == string));
    static assert(is(typeof(r3.post) == string));
    assert(!r3);
    assert(r3.pre == "a+b*c");
    assert(r3.separator == []);
    assert(r3.post == []);
}
