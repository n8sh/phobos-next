module splitter_ex;

import std.traits : isExpressions;

/** Non-decoding ASCII-separator-only variant of Phobos' `splitter`. */
template splitterASCII(alias separatorPred)
{
    auto splitterASCII(Range)(return Range r) @trusted
    if (is(typeof(Range.init[0 .. 0])) && // can be sliced
        is(typeof(Range.init[0]) : char) &&
        is(typeof(separatorPred(char.init)) : bool)) // TODO check that first parameter is bool
    {
        static struct Result
        {
            private Range _input; // original copy of r
            private size_t _offset = 0; // hit offset if any, or `_haystack.length` if miss

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

            void skipSeparators() @trusted
            {
                while (_offset < _input.length &&
                       separatorPred(_input.ptr[_offset]))
                {
                    /* predicate `separatorPred` must only filter out ASCII, or
                     * incorrect UTF-8 decoding will follow */
                    assert(_input.ptr[_offset].isASCII);
                    _offset += 1;
                }
                _input = _input[_offset .. $]; // skip leading separators
                _offset = 0;
            }

            void findNext() @trusted
            {
                while (_offset < _input.length &&
                       !separatorPred(_input.ptr[_offset]))
                {
                    _offset += 1;
                }
                // dbg("input:", _input, " ", " offset:", _offset);
            }

            void popFront() nothrow
            {
                assert(!empty, "Attempting to pop the front of an empty splitter.");
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
    import std.algorithm.comparison : among;
    // foreach (part; ` a b c-_d`.splitterASCII!(' ', '-', '_'))
    // {
    //     dbg("`", part, "`");
    // }
    assert(``.splitterASCII!(_ => _ == ' ').empty);
    assert(` `.splitterASCII!(_ => _ == ' ').empty);
    assert(`   `.splitterASCII!(_ => _ == ' ').empty);
    assert(` - `.splitterASCII!(_ => _ == ' ').equal([`-`].s[]));
    assert(`a`.splitterASCII!(_ => _ == ' ').equal([`a`].s[]));
    assert(` a `.splitterASCII!(_ => _ == ' ').equal([`a`].s[]));
    assert(` a b `.splitterASCII!(_ => _ == ' ').equal([`a`, `b`].s[]));
    assert(` a_b `.splitterASCII!(_ => _ == ' ').equal([`a_b`].s[]));
    assert(` - aa   bb--c-_d--_e`.splitterASCII!(_ => _.among!(' ', '-', '_') != 0).equal([`aa`, `bb`, `c`, `d`, `e`].s[]));
}

private bool isASCII(char x) @safe pure nothrow @nogc
{
    return x < 128;
}

version(unittest)
{
    import array_help : s;
    import dbgio;
}
