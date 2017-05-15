/** Structure of arrays.
    See also: https://maikklein.github.io/post/soa-d/
    See also: TODO Add my forum post
    TODO is it possible to cleverly allow syntax x[0].name to not have to create
 */
module soa;

@safe /*pure*/:

/** Structure of arrays similar to members of `S`.
 */
struct SOA(S)
    if (is(S == struct))        // TODO extend to `isAggregate!S`
{
    import std.experimental.allocator;
    import std.experimental.allocator.mallocator;

    import std.meta : staticMap;
    import std.traits : FieldNameTuple;

    alias toArray(S) = S[];
    alias toType(string s) = typeof(__traits(getMember, S, s));

    alias MemberNames = FieldNameTuple!S;
    enum memberCount = MemberNames.length;
    alias Types = staticMap!(toType, MemberNames);

    @safe /*pure*/:

    this(size_t size_,
         IAllocator _alloc = allocatorObject(Mallocator.instance))
    {
        _alloc = _alloc;
        _capacity = size_;
        allocate(size_);
    }

    auto ref opDispatch(string name)()
    {
        import std.meta : staticIndexOf;
        alias index = staticIndexOf!(name, MemberNames);
        static assert(index >= 0);
        return getContainer!index;
    }

    void pushBackMembers(Types types)
    {
        if (_length == _capacity) { grow(); }
        foreach (const index, _; MemberNames)
        {
            // TODO functionize
            static if (false)
            {
                import std.algorithm.mutation : move;
                move(types[index], getContainer!index[_length]);
            }
            else
            {
                getContainer!index[_length] = types[index];
            }
        }
        ++_length;
    }

    void pushBack(S e)
    {
        if (_length == _capacity) { grow(); }
        foreach (const index, _; MemberNames)
        {
            // TODO functionize
            static if (false)
            {
                import std.algorithm.mutation : move;
                move(__traits(getMember, e, MemberNames[index]), getContainer!index[_length]);
            }
            else
            {
                getContainer!index[_length] = __traits(getMember, e, MemberNames[index]);
            }
        }
        ++_length;
    }

    void opOpAssign(string op, S)(S e)
        if (op == "~")
    {
        import std.algorithm.mutation : move;
        pushBack(move(e));      // TODO remove when compile does this for us
    }

    size_t length() const @property
    {
        return _length;
    }

    ~this() @trusted
    {
        if (_alloc is null) { return; }
        foreach (const index, _; MemberNames)
        {
            _alloc.dispose(getContainer!index);
        }
    }

private:

    void length(size_t newLength) @property
    {
        _length = newLength;
    }

    // TODO use template mixin and getContainer instead of Tuple for better performance
    // import std.typecons : Tuple;
    // alias ArrayTypes = staticMap!(toArray, Types);
    // Tuple!ArrayTypes containers;

    static string generateContainers()
    {
        string defs;
        foreach (const index, Type; Types)
        {
            enum TypeName = Type.stringof;
            defs ~= TypeName ~ `[] container` ~ index.stringof ~ ";";
        }
        return defs;
    }
    mixin(generateContainers());

    ref inout(Types[index][]) getContainer(size_t index)() inout return scope
    {
        mixin(`return ` ~ `container` ~ index.stringof ~ ";");
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
        foreach (const index, _; MemberNames)
        {
            getContainer!index = _alloc.makeArray!(Types[index])(newCapacity);
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
                _alloc.expandArray(getContainer!index, expandSize);
            }
        }
        _capacity = newCapacity;
    }
}

unittest
{
    struct S { int i; float f; }

    auto x = SOA!S();

    static assert(is(typeof(x.getContainer!0()) == int[]));
    static assert(is(typeof(x.getContainer!1()) == float[]));

    assert(x.length == 0);

    x.pushBack(S.init);
    assert(x.length == 1);

    x ~= S.init;
    assert(x.length == 2);

    x.pushBackMembers(42, 43f);
    assert(x.length == 3);
    assert(x.i[2] == 42);
    assert(x.f[2] == 43);

    auto x3 = SOA!S(3);
    assert(x3.length == 0);
}
