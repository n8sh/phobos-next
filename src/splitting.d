module splitting;

import std.range : isForwardRange, ElementType, hasSlicing, hasLength, isNarrowString, isBidirectionalRange, isInfinite, ElementEncodingType, empty, save, front;
import std.functional : binaryFun;

auto splitter(alias pred = "a == b", Range, Separator)(Range r, Separator s)
    if (is(typeof(binaryFun!pred(r.front, s)) : bool)
        && ((hasSlicing!Range && hasLength!Range) || isNarrowString!Range))
{
    import std.algorithm.searching : find;
    import std.conv : unsigned;

    static struct Result
    {
    private:
        Range _input;
        Separator _separator;
        // Do we need hasLength!Range? popFront uses _input.length...
        enum size_t _unComputed = size_t.max - 1, _atEnd = size_t.max;
        size_t _frontLength = _unComputed;
        size_t _backLength = _unComputed;

        static if (isNarrowString!Range)
        {
            size_t _separatorLength;
        }
        else
        {
            enum _separatorLength = 1;
        }

        static if (isBidirectionalRange!Range)
        {
            static size_t lastIndexOf(Range haystack, Separator needle)
            {
                import std.range : retro;
                auto r = haystack.retro().find!pred(needle);
                return r.retro().length - 1;
            }
        }

    public:
        this(Range input, Separator separator)
        {
            _input = input;
            _separator = separator;

            static if (isNarrowString!Range)
            {
                import std.utf : codeLength;

                _separatorLength = codeLength!(ElementEncodingType!Range)(separator);
            }
            if (_input.empty)
                _frontLength = _atEnd;
        }

        static if (isInfinite!Range)
        {
            enum bool empty = false;
        }
        else
        {
            @property bool empty()
            {
                return _frontLength == _atEnd;
            }
        }

        @property Range front()
        {
            assert(!empty, "Attempting to fetch the front of an empty splitter.");
            if (_frontLength == _unComputed)
            {
                auto r = _input.find!pred(_separator);
                _frontLength = _input.length - r.length;
            }
            return _input[0 .. _frontLength];
        }

        void popFront()
        {
            assert(!empty, "Attempting to popFront an empty splitter.");
            if (_frontLength == _unComputed)
            {
                front;
            }
            assert(_frontLength <= _input.length);
            if (_frontLength == _input.length)
            {
                // no more input and need to fetch => done
                _frontLength = _atEnd;

                // Probably don't need this, but just for consistency:
                _backLength = _atEnd;
            }
            else
            {
                _input = _input[_frontLength + _separatorLength .. _input.length];
                _frontLength = _unComputed;
            }
        }

        static if (isForwardRange!Range)
        {
            @property typeof(this) save()
            {
                auto ret = this;
                ret._input = _input.save;
                return ret;
            }
        }

        static if (isBidirectionalRange!Range)
        {
            @property Range back()
            {
                assert(!empty, "Attempting to fetch the back of an empty splitter.");
                if (_backLength == _unComputed)
                {
                    immutable lastIndex = lastIndexOf(_input, _separator);
                    if (lastIndex == -1)
                    {
                        _backLength = _input.length;
                    }
                    else
                    {
                        _backLength = _input.length - lastIndex - 1;
                    }
                }
                return _input[_input.length - _backLength .. _input.length];
            }

            void popBack()
            {
                assert(!empty, "Attempting to popBack an empty splitter.");
                if (_backLength == _unComputed)
                {
                    // evaluate back to make sure it's computed
                    back;
                }
                assert(_backLength <= _input.length);
                if (_backLength == _input.length)
                {
                    // no more input and need to fetch => done
                    _frontLength = _atEnd;
                    _backLength = _atEnd;
                }
                else
                {
                    _input = _input[0 .. _input.length - _backLength - _separatorLength];
                    _backLength = _unComputed;
                }
            }
        }
    }

    return Result(r, s);
}

///
@safe pure /*nothrow @nogc*/ unittest
{
    import std.algorithm.comparison : equal;

    assert(equal(splitter("hello  world", ' '), [ "hello", "", "world" ]));
    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [], [3], [4, 5], [] ];
    assert(equal(splitter(a, 0), w));
    a = [ 0 ];
    assert(equal(splitter(a, 0), [ (int[]).init, (int[]).init ]));
    a = [ 0, 1 ];
    assert(equal(splitter(a, 0), [ [], [1] ]));
    w = [ [0], [1], [2] ];
    assert(equal(splitter!"a.front == b"(w, 1), [ [[0]], [[2]] ]));
}

@safe pure nothrow @nogc unittest
{
    // auto x = [1,2,3, 5, 10,11].s[].splitterAdjacent!((a, b) => a + 1 != b);
    // pragma(msg, ElementType!(typeof(x)));
    // assert(x.equal([[1,2,3].s,
    //                 [5].s,
    //                 [10,11].s].s));
}

version(unittest)
{
    import array_help : s;
}
