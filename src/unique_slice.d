module unique_slice;

@safe pure:

/** Unique slice owning its source of `Source`.
    Copy construction is disabled.
 */
struct UniqueSlice(Source)
{
    alias Slice = typeof(Source.init[]);

    @disable this(this);

    this(Source source) @trusted
    {
        import std.algorithm.mutation : move;
        _source = move(source); // TODO remove `move` when compiler does it for us
        _slice = source[];
    }

    alias _slice this;

    Slice _slice;
    Source _source; // typically a non-reference count container type with disable copy construction
}

/** Returns: A slice of `Source` that own it's `source` (data container).
    Similar to Rust's `into_iter`.
 */
pragma(inline) UniqueSlice!Source intoSlice(Source)(Source source)
{
    import std.algorithm.mutation : move;
    return typeof(return)(move(source)); // TODO remove `move` when compiler does it for us
}

///
nothrow @nogc unittest
{
    import array_ex : SA = UncopyableArray;
    alias C = SA!int;
    auto cs = C.withElements(1, 3, 5, 7).intoSlice;
    foreach (e; cs)
    {
    }
}
