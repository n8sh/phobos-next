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
            private size_t _offset = 0; // hit offset if any, or `_haystack.length` if miss

            import std.algorithm.comparison : among;

            this(Range input)
            {
                // dbg("input:", input);
                _input = input;

                skipSeparators(); // skip leading separators

                findNext();
            }

            bool empty() const
            {
                // dbg("input:", _input, " ", " offset:", _offset);
                return _input.length == 0;
            }

            @property Range front()
            {
                // dbg("input:", _input, " ", " offset:", _offset);
                assert(!empty, "Attempting to fetch the front of an empty splitter.");
                return _input[0 .. _offset];
            }

            void skipSeparators()
            {
                while (_offset < _input.length &&
                       _input[_offset].among!(separators))
                {
                    _offset += 1;
                }
                _input = _input[_offset .. $]; // skip leading separators
                _offset = 0;
            }

            void findNext()
            {
                while (_offset < _input.length &&
                       !_input[_offset].among!(separators))
                {
                    _offset += 1;
                }
                // dbg("input:", _input, " ", " offset:", _offset);
            }

            void popFront()
            {
                skipSeparators();
                findNext();
            }
        }

        return Result(r);
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.algorithm.comparison : equal;
    // foreach (part; ` a b c-_d`.splitterAmongASCII!(' ', '-', '_'))
    // {
    //     dbg("`", part, "`");
    // }
    assert(`a`.splitterAmongASCII!(' ').equal([`a`].s[]));
    assert(` a `.splitterAmongASCII!(' ').equal([`a`].s[]));
    assert(` - aa   bb--c-_d--`.splitterAmongASCII!(' ', '-', '_')
                       .equal([`aa`, `bb`, `c`, `d`].s[]));
}

version(unittest)
{
    import array_help : s;
    import dbgio;
}
