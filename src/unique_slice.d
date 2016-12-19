module unique_slice;

@safe pure:

struct UniqueSlice(Source)
{
    alias Slice = typeof(Source.init[]);
    @disable this(this);
    Source _source; // typically a non-reference count container type with disable copy construction
}

UniqueSlice!Source intoSlice(Source)(Source source)
{
    import std.algorithm.mutation : move;
    return typeof(return)(move(source)); // TODO remove `move` when compiler does it for us
}

nothrow @nogc unittest
{
    import array_ex : SA = UncopyableArray;
    alias C = SA!int;
    auto cs = C.withElements(1, 3, 5, 7).intoSlice;
}
