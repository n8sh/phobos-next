module basic_uncopyable_array;

import std.traits : Unqual;

/** Non-copyable variant of `CopyableArray`.
 */
struct UncopyableArray(T,
                       alias Allocator = null, // null means means to qcmeman functions
                       CapacityType = size_t)  // see also https://github.com/izabera/s
    if (!is(Unqual!T == bool) &&             // use `BitArray` instead
        (is(CapacityType == ulong) ||        // 3 64-bit words
         is(CapacityType == uint)))          // 2 64-bit words
{
    import std.range : ElementType, isCopyable;

    pragma(inline, true):

    /// Returns: an array of length `initialLength` with all elements default-initialized to `ElementType.init`.
    static typeof(this) withLength(size_t initialLength) @trusted
    {
        typeof(return) that;
        that._basicArray = Super.withLength(initialLength);
        return that;
    }

    /// Construct from element `values`.
    this(U)(U[] values...)
        if (Super.isElementAssignable!U &&
            isCopyable!U)       // prevent accidental move of l-value `values`
    {
        _basicArray = Super(values);
    }

    /// Construct from range of element `values`.
    pragma(inline)              // DMD cannot inline
    this(R)(R values)
        if (Super.isAssignableFromElementsOfRefIterableStruct!R)
    {
        _basicArray = Super(values);
    }

    /// Construct from uncopyable range of uncopyable `values`.
    this(R)(R values) @trusted
        if (!isCopyable!R &&
            !isCopyable!(ElementType!R) &&
            Super.isElementAssignable!(ElementType!R))
    {
        static assert(0, "TODO implement");
    }

    @disable this(this);        // no copy construction

    /// Returns: shallow duplicate of `this`.
    static if (isCopyable!T)
    {
        // `MutableThis` mimics behaviour of `dup` for builtin D arrays
        pragma(inline)          // DMD cannot inline
        @property UncopyableArray!(Unqual!T, Allocator, CapacityType) dup() const @trusted
        {
            return typeof(return)(cast(Unqual!T[])this[]);
        }
    }

    import basic_copyable_array : CopyableArray;
    alias Super = CopyableArray!(T, Allocator, CapacityType);
    Super _basicArray;
    alias _basicArray this;
}

/// construct from uncopyable scalar
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = UncopyableArray!(T, null, uint);
    const a = A(17);
    assert(a[] == [17].s);
}

/// construct from slice of copyable type
@safe pure nothrow unittest
{
    alias T = int;
    alias A = UncopyableArray!(T);
    const a = A([17]);
    assert(a[] == [17].s);
}

/// check duplication
@safe pure nothrow @nogc unittest
{
    alias T = int;
    alias A = UncopyableArray!(T);

    static assert(!__traits(compiles, { A b = a; })); // copying disabled

    auto a = A([10, 11, 12].s);
    auto b = a.dup;
    assert(a == b);
    assert(a[].ptr !is b[].ptr);
}

/// construct from map range
@safe pure nothrow unittest
{
    import std.algorithm : map;
    alias T = int;
    alias A = UncopyableArray!(T);
    auto a = A([10, 20, 30].s[].map!(_ => _^^2));
    assert(a[] == [100, 400, 900].s);
}

version(unittest)
{
    import array_help : s;
}
