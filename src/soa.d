/** Structure of arrays.

    See also: https://maikklein.github.io/post/soa-d/
    See also: http://forum.dlang.org/post/wvulryummkqtskiwrusb@forum.dlang.org
 */
module soa;

@safe /*TODO pure*/:

/** Structure of arrays similar to members of `S`.
 */
struct SOA(S)
    if (is(S == struct))        // TODO extend to `isAggregate!S`?
{
    import std.experimental.allocator;
    import std.experimental.allocator.mallocator;

    import std.meta : staticMap;
    import std.traits : FieldNameTuple;

    alias toArray(S) = S[];
    alias toType(string s) = typeof(__traits(getMember, S, s));
    alias toPtrType(string s) = typeof(__traits(getMember, S, s));
    alias ElementType = S;

    alias MemberNames = FieldNameTuple!S;
    alias Types = staticMap!(toType, MemberNames);
    alias PtrTypes = staticMap!(toPtrType, MemberNames);

    @safe /*pure*/:

    this(size_t initialCapacity,
         IAllocator _alloc = allocatorObject(Mallocator.instance))
    {
        _alloc = _alloc;
        _capacity = initialCapacity;
        allocate(initialCapacity);
    }

    auto opDispatch(string name)()
    {
        import std.meta : staticIndexOf;
        alias index = staticIndexOf!(name, MemberNames);
        static assert(index >= 0);
        return getArray!index;
    }

    void insertBackMembers(Types types)
    {
        if (_length == _capacity) { grow(); }
        foreach (const index, _; MemberNames)
        {
            import std.algorithm.mutation : move;
            move(types[index], getArray!index[_length]); // same as `getArray!index[_length] = types[index];`
        }
        ++_length;
    }

    /// Push element `e` to back of array.
    void insertBack(S e)
    {
        if (_length == _capacity) { grow(); }
        foreach (const index, _; MemberNames)
        {
            import std.algorithm.mutation : move;
            move(__traits(getMember, e, MemberNames[index]), getArray!index[_length]); // same as `getArray!index[_length] = __traits(getMember, e, MemberNames[index]);`
        }
        ++_length;
    }

    void opOpAssign(string op, S)(S e)
        if (op == "~")
    {
        import std.algorithm.mutation : move;
        insertBack(move(e));      // TODO remove when compiler does this for us
    }

    /// Length of this array.
    size_t length() const @property
    {
        return _length;
    }

    /// Capacity of this array.
    size_t capacity() const @property
    {
        return _capacity;
    }

    ~this() @trusted
    {
        if (_alloc is null) { return; }
        foreach (const index, _; MemberNames)
        {
            _alloc.dispose(getArray!index);
        }
    }

    /** Index operator. */
    inout(SOAElementRef!S) opIndex(size_t elementIndex) inout return
    {
        assert(elementIndex < _length);
        return typeof(return)(&this, elementIndex);
    }

private:

    // TODO use when importing std.typecons doesn't cost to performance
    // import std.typecons : Tuple;
    // alias ArrayTypes = staticMap!(toArray, Types);
    // Tuple!ArrayTypes containers;

    static string generateArrays()
    {
        version(LDC) static if (__VERSION__ >= 2076) { static assert(0, "TODO use static foreach"); }
        string defs;
        foreach (const index, Type; Types)
        {
            enum TypeName = Type.stringof;
            defs ~= TypeName ~ `[] _container` ~ index.stringof ~ ";";
        }
        return defs;
    }
    mixin(generateArrays());

    ref inout(Types[index][]) getArray(size_t index)() inout return
    {
        mixin(`return _container` ~ index.stringof ~ ";");
    }

    IAllocator _alloc;

    size_t _length = 0;
    size_t _capacity = 0;
    short growFactor = 2;

    void allocate(size_t newCapacity) @trusted
    {
        if (_alloc is null)
        {
            _alloc = allocatorObject(Mallocator.instance);
        }
        version(LDC) static if (__VERSION__ >= 2076) { static assert(0, "TODO use static foreach"); }
        foreach (const index, _; MemberNames)
        {
            getArray!index = _alloc.makeArray!(Types[index])(newCapacity);
        }
    }

    void grow() @trusted
    {
        import std.algorithm: max;
        size_t newCapacity = max(1, _capacity * growFactor);
        size_t expandSize = newCapacity - _capacity;

        if (_capacity is 0)
        {
            allocate(newCapacity);
        }
        else
        {
            foreach (const index, _; MemberNames)
            {
                _alloc.expandArray(getArray!index, expandSize);
            }
        }
        _capacity = newCapacity;
    }
}

/// Reference to element in `soaPtr` at index `elementIndex`.
private struct SOAElementRef(S)
    if (is(S == struct))        // TODO extend to `isAggregate!S`?
{
    SOA!S* soaPtr;
    size_t elementIndex;

    @disable this(this);

    /// Access member name `memberName`.
    auto ref opDispatch(string nameName)()
        @trusted return scope
    {
        mixin(`return ` ~ `(*soaPtr).` ~ nameName ~ `[elementIndex];`);
    }
}

unittest
{
    struct S { int i; float f; }

    auto x = SOA!S();

    static assert(is(typeof(x.getArray!0()) == int[]));
    static assert(is(typeof(x.getArray!1()) == float[]));

    assert(x.length == 0);

    x.insertBack(S.init);
    assert(x.length == 1);

    x ~= S.init;
    assert(x.length == 2);

    x.insertBackMembers(42, 43f);
    assert(x.length == 3);
    assert(x.i[2] == 42);
    assert(x.f[2] == 43f);

    // uses opDispatch
    assert(x[2].i == 42);
    assert(x[2].f == 43f);

    const x3 = SOA!S(3);
    assert(x3.length == 0);
    assert(x3.capacity == 3);

    // disable for now because -dip1000 cannot be set in dub.sdl because it
    // transitively affects depending packages
    static assert(!__traits(compiles,
                            {
                                ref int testScope() @safe
                                {
                                    auto y = SOA!S(1);
                                    y ~= S(42, 43f);
                                    return y[0].i;
                                }
                            }));
}
