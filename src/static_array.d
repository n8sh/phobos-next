module static_array;

/** Statically allocated `E`-array of fixed pre-allocated length.
    Similar to Rust's `fixedvec`: https://docs.rs/fixedvec/0.2.3/fixedvec/
*/
struct ArrayN(E, uint capacity)
{
    import std.traits : isSomeChar;

    E[capacity] _store = void;  /// stored elements

    /// number of elements in `_store`
    static      if (capacity < ubyte.max + 1)  ubyte _length;
    else static if (capacity < ushort.max + 1) ushort _length;
    else static assert("Too large capacity " ~ capacity);

    alias ElementType = E;

    @safe pure nothrow @nogc pragma(inline):

    /** Construct with elements `es`. */
    this(Es...)(Es es)
        if (Es.length >= 1 &&
            Es.length <= capacity)
    {
        foreach (const i, const ix; es)
        {
            import std.algorithm.mutation : move;
            _store[i] = ix.move(); // move
        }
        _length = es.length;
    }

    /** Construct with elements in `es`. */
    this(const E[] es)
    {
        assert(es.length <= capacity);
        _store[0 .. es.length] = es; // copy
        _length = cast(ubyte)es.length;
    }

    /** Returns: `true` if `this` is empty, `false` otherwise. */
    @property bool empty() const { return _length == 0; }

    /** Returns: `true` if `this` is full, `false` otherwise. */
    @property bool full() const { return _length == capacity; }

    /** Get length. */
    @property auto length() const { return _length; }
    alias opDollar = length;    /// ditto

    /** Get as `string`. */
    @property auto toString() const @system // TODO DIP-1000 scope
    {
        static if (isSomeChar!E) { return opSlice(); }
    }

inout:

    /** Index operator. */
    ref inout(E) opIndex(size_t i) // TODO DIP-1000 scope
    {
        assert(i < _length);
        return _store[i];
    }

    /** First (front) element. */
    ref inout(E) front()        // TODO DIP-1000 scope
    {
        assert(!empty);
        return _store[0];
    }

    /** Last (back) element. */
    ref inout(E) back()         // TODO DIP-1000 scope
    {
        assert(!empty);
        return _store[_length - 1];
    }

    /** Slice operator. */
    inout(E)[] opSlice() @system    // TODO DIP-1000 scope
    {
        return opSlice(0, _length);
    }
    /** ditto */
    inout(E)[] opSlice(size_t i, size_t j) @system // TODO DIP-1000 scope
    {
        assert(i <= j);
        assert(j <= _length);
        return _store.ptr[i .. j];
    }
}

/** Stack-allocated string of maximum length of `capacity.` */
alias StringN(uint capacity) = ArrayN!(immutable(char), capacity);
/** Stack-allocated wstring of maximum length of `capacity.` */
alias WStringN(uint capacity) = ArrayN!(immutable(wchar), capacity);
/** Stack-allocated dstring of maximum length of `capacity.` */
alias DStringN(uint capacity) = ArrayN!(immutable(dchar), capacity);

version(unittest)
{
    import std.algorithm.comparison : equal;
}

///
pure unittest
{
    alias E = char;
    enum capacity = 3;

    alias A = ArrayN!(E, capacity);
    static assert(A.sizeof == E.sizeof*capacity + 1);

    auto ab = A('a', 'b');
    assert(!ab.empty);
    assert(ab[0] == 'a');
    assert(ab.front == 'a');
    assert(ab.back == 'b');
    assert(ab.length == 2);
    assert(ab[] == "ab");
    assert(ab[0 .. 1] == "a");

    const abc = A('a', 'b', 'c');
    assert(!abc.empty);
    assert(abc.front == 'a');
    assert(abc.back == 'c');
    assert(abc.length == 3);
    assert(abc[] == "abc");
    assert(ab[0 .. 2] == "ab");
    assert(abc.full);
    static assert(!__traits(compiles, { const abcd = A('a', 'b', 'c', 'd'); }));

    const xy = A("xy");
    assert(!xy.empty);
    assert(xy[0] == 'x');
    assert(xy.front == 'x');
    assert(xy.back == 'y');
    assert(xy.length == 2);
    assert(xy[] == "xy");
    assert(xy[0 .. 1] == "x");

    const xyz = A('x', 'y', 'z');
    assert(!xyz.empty);
    assert(xyz.front == 'x');
    assert(xyz.back == 'z');
    assert(xyz.length == 3);
    assert(xyz[] == "xyz");
    assert(ab[0 .. 2] == "ab");
    assert(xyz.full);
    static assert(!__traits(compiles, { const xyzw = A('x', 'y', 'z', 'w'); }));
}

void testAsSomeString(E)()
{
    enum capacity = 15;
    alias A = ArrayN!(immutable(E), capacity);
    const a = A("abc");
    assert(a[] == "abc");
    assert(a[].equal("abc"));
}

/// strings
pure unittest
{
    import std.typecons : AliasSeq;
    foreach (E; AliasSeq!(char, wchar, dchar))
    {
        testAsSomeString!E();
    }
}
