module splitter_ex;

import std.traits : isExpressions;

/** Non-decoding ASCII-needle-only variant of Phobos' `splitter`. */
template splitterAmongASCII(separators...)
if (separators.length != 0 &&
    isExpressions!separators)
{
    import std.meta : allSatisfy;
    import char_traits : isASCII;

    auto splitterAmongASCII(Range)(return Range r) @trusted
        if (is(typeof(Range.init[0 .. 0])) && // can be sliced
            is(typeof(Range.init[0]) : char) &&
            allSatisfy!(isASCII, separators))
    {
        static struct Result
        {
            private Range _input; // original copy of r
            private size_t _frontLength = 0; // hit offset if any, or `_haystack.length` if miss

            this(Range input)
            {
                _input = input;
                // TODO skip needleds
            }

            bool empty() @safe pure nothrow @nogc
            {
                return _input.length == 0;
            }

            @property Range front()
            {
                assert(!empty, "Attempting to fetch the front of an empty splitter.");
                return _input[0 .. _frontLength];
            }

            void popFront()
            {
            }
        }

        return Result(r);
    }
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    assert(`a b c-_d`.splitterAmongASCII!(' ', '_')
                     .equal([`a`, `b`, `c`, `d`]));
}
