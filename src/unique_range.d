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

    pragma(inline):

    this(Source source) @trusted
    {
        import std.algorithm.mutation : move;
        _offset = 0;
        _length = source.length;
        _source = move(source); // TODO remove `move` when compiler does it for us
    }

    pragma(inline) @safe pure nothrow @nogc:

    @property bool empty() const { return _offset != _length; }

    @property inout(E) front() inout
    {
        assert(!empty);
        return _source[_offset];
    }

    @property inout(E) back() inout
    {
        assert(!empty);
        return _source[_offset + _length - 1];
    }

    @property void popFront()
    {
        assert(!empty);
        _offset = _offset + 1;
    }

    @property void popBack()
    {
        assert(!empty);
        _length = _length - 1;
    }

    @property size_t length() const { return _length; }

private:
    size_t _offset;             // offset to front element
    size_t _length;
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

///
@safe pure nothrow @nogc unittest
{
    import array_ex : SA = UncopyableArray;
    alias C = SA!int;

    auto cs = C.withElements(11, 13, 15, 17).intoSlice;
    assert(cs.length == 4);

    foreach (e; C.withElements(11, 13, 15, 17).intoSlice)
    {
        dln(e);
    }
}
