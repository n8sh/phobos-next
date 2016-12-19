module unique_range;

version(unittest)
{
    import dbgio : dln;
}

import std.range.primitives : hasLength;

/** Unique slice owning its source of `Source`.
    Copy construction is disabled.
 */
struct UniqueArrayRange(Source)
    if (hasLength!Source)       // TODO use traits `isArrayContainer`
{
    import std.range : ElementType;
    alias Slice = typeof(Source.init[]);
    alias E = ElementType!Slice;

    @disable this(this);

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

    /// Length.
    @property size_t length() const { return _backIx - _frontIx; }

private:
    size_t _frontIx;             // offset to front element
    size_t _backIx;
    Source _source; // typically a non-reference count container type with disable copy construction
}

/** Returns: A slice of `Source` that own it's `source` (data container).
    Similar to Rust's `into_iter`.
 */
UniqueArrayRange!Source intoSlice(Source)(Source source)
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

    auto cs = C.withElements(11, 13, 15, 17).intoSlice;

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

/// basics
@safe pure nothrow @nogc unittest
{
    import std.algorithm.iteration : map, filter;

    import array_ex : SA = UncopyableArray;
    alias C = SA!int;

    version(none) // TODO is proven to work when `map` and `filter` accepts non-copyable parameters
    {
        foreach (ref e; C.withElements(11, 13, 15, 17)
                         .intoSlice
                         .map!(_ => _^^2)
                         .filter!(_ => _ != 121))
        {
            dln(e);
        }
    }
}
