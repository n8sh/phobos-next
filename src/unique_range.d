module unique_range;

version(unittest)
{
    import dbgio : dln;
    import std.algorithm.comparison : equal;
}

import std.range.primitives : hasLength;

/** Unique range (slice) owning its source of `Source`.

    Copy construction is disabled, explicit copying is instead done through
    member `.dup`.
 */
struct UniqueRange(Source)
    if (hasLength!Source)       // TODO use traits `isArrayContainer`
{
    import std.range : ElementType;
    alias Slice = typeof(Source.init[]);
    alias E = ElementType!Slice;

    @disable this(this);        // not intended to be copied

    pragma(inline) @safe pure nothrow @nogc:

    /// Construct from `source`.
    this(Source source)
    {
        import std.algorithm.mutation : move;
        _frontIx = 0;
        _backIx = source.length;
        _source = move(source); // TODO remove `move` when compiler does it for us
    }

    /// Is `true` if range is empty.
    @property bool empty() const { return _frontIx == _backIx; }

    /// Front element.
    @property inout(E) front() inout
    {
        assert(!empty);
        return _source[_frontIx];
    }

    /// Back element.
    @property inout(E) back() inout
    {
        assert(!empty);
        return _source[_backIx - 1];
    }

    /// Pop front element.
    @property void popFront()
    {
        assert(!empty);
        _frontIx = _frontIx + 1;
    }

    /// Pop back element.
    @property void popBack()
    {
        assert(!empty);
        _backIx = _backIx - 1;
    }

    /// Returns: shallow duplicate of `this`.
    version(none)               // TODO make compile
    {
        @property UniqueRange dup() const
        {
            return typeof(this)(_frontIx, _backIx, _source.dup);
        }
    }

    /// Length.
    @property size_t length() const { return _backIx - _frontIx; }

private:
    size_t _frontIx;             // offset to front element
    size_t _backIx;
    Source _source; // typically a non-reference count container type with disable copy construction
}

/** Returns: A range of `Source` that owns its `source` (data container).
    Similar to Rust's `into_iter`.
 */
UniqueRange!Source intoRange(Source)(Source source)
    if (hasLength!Source)
{
    import std.algorithm.mutation : move;
    return typeof(return)(move(source)); // TODO remove `move` when compiler does it for us
}

/// basics
@safe pure nothrow @nogc unittest
{
    import array_ex : SA = UncopyableArray;
    alias C = SA!int;

    auto cs = C.withElements(11, 13, 15, 17).intoRange;

    assert(!cs.empty);
    assert(cs.length == 4);
    assert(cs.front == 11);
    assert(cs.back == 17);

    cs.popFront();
    assert(cs.length == 3);
    assert(cs.front == 13);
    assert(cs.back == 17);

    cs.popBack();
    assert(cs.length == 2);
    assert(cs.front == 13);
    assert(cs.back == 15);

    cs.popFront();
    assert(cs.length == 1);
    assert(cs.front == 15);
    assert(cs.back == 15);

    cs.popBack();
    assert(cs.length == 0);
    assert(cs.empty);
}

/// combined with Phobos ranges
@safe pure nothrow unittest
{
    import array_ex : SA = UncopyableArray;
    alias C = SA!int;

    equal(C.withElements(11, 13, 15, 17)
           .intoRange
           .filter!(_ => _ != 11),
          [13, 15, 17]);
}

import std.functional : unaryFun;

template filter(alias predicate) if (is(typeof(unaryFun!predicate)))
{
    import std.algorithm.mutation : move, moveEmplace;
    import std.range.primitives : isInputRange, isForwardRange, isInfinite;
    import std.traits : Unqual, isCopyable;

    private static struct Result(alias pred, Range)
    {
        alias R = Unqual!Range;
        R _input;

        this(R r)
        {
            move(r, _input);
            while (!_input.empty && !pred(_input.front))
            {
                _input.popFront();
            }
        }

        static if (isCopyable!Range)
        {
            auto opSlice() { return this; }
        }

        static if (isInfinite!Range)
        {
            enum bool empty = false;
        }
        else
        {
            @property bool empty() { return _input.empty; }
        }

        void popFront()
        {
            do
            {
                _input.popFront();
            } while (!_input.empty && !pred(_input.front));
        }

        @property auto ref front()
        {
            assert(!empty, "Attempting to fetch the front of an empty filter.");
            return _input.front;
        }

        static if (isForwardRange!R)
        {
            @property auto save()
            {
                return typeof(this)(_input.save);
            }
        }
    }

    auto filter(Range)(Range range) if (isInputRange!(Unqual!Range))
    {
        return Result!(unaryFun!predicate, Range)(move(range));
    }
}
