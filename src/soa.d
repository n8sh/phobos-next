/** Structure of arrays.

    See_Also: https://maikklein.github.io/post/soa-d/
    See_Also: http://forum.dlang.org/post/wvulryummkqtskiwrusb@forum.dlang.org
 */
module soa;

@safe /*TODO pure*/:

/** Structure of arrays similar to members of `S`.
 */
struct SOA(S)
if (is(S == struct))        // TODO extend to `isAggregate!S`?
{
    import pure_mallocator : PureMallocator;

    private alias toType(string s) = typeof(__traits(getMember, S, s));
    private alias Types = typeof(S.tupleof);

    this(size_t initialCapacity) 
    {
        _capacity = initialCapacity;
        allocate(initialCapacity);
    }

    auto opDispatch(string name)()
    {
        static foreach (index, memberSymbol; S.tupleof)
        {
            static if (name == memberSymbol.stringof)
            {
                return getArray!index;
            }
        }
        // TODO static assert(0, S.stringof ~ " has no field named " ~ name);
    }

    /// Push element (struct) `value` to back of array.
    void insertBack()(S value)  // template-lazy
    {
        reserveOneExtra();
        import core.lifetime : move;
        static foreach (const index, memberSymbol; S.tupleof)
        {
            move(__traits(getMember, value, memberSymbol.stringof),
                 getArray!index[_length]);
        }
        ++_length;
    }

    /// Push element (struct) `value` to back of array using its data members `members`.
    void insertBackMembers()(Types members) // template-lazy
    {
        reserveOneExtra();
        import core.lifetime : move;
        static foreach (const index, _; members)
        {
            move(members[index], getArray!index[_length]); // same as `getArray!index[_length] = members[index];`
        }
        ++_length;
    }

    void opOpAssign(string op, S)(S value)
        if (op == "~")
    {
        import core.lifetime : move;
        insertBack(move(value));      // TODO remove when compiler does this for us
    }

    /// Length of this array.
    @property size_t length() const @safe pure nothrow @nogc
    {
        return _length;
    }

    /// Capacity of this array.
    @property size_t capacity() const @safe pure nothrow @nogc
    {
        return _capacity;
    }

    ~this() @trusted
    {
        import std.experimental.allocator : dispose;
        static foreach (const index, _; S.tupleof)
        {
            PureMallocator.instance.dispose(getArray!index);
        }
    }

    /** Index operator. */
    inout(SOAElementRef!S) opIndex()(size_t elementIndex) inout return // template-lazy
    {
        assert(elementIndex < _length);
        return typeof(return)(&this, elementIndex);
    }

private:

    // generate array definitions
    static foreach (index, Type; Types)
    {
        mixin(Type.stringof ~ `[] _container` ~ index.stringof ~ ";");
    }

    ref inout(Types[index][]) getArray(size_t index)() inout return
    {
        mixin(`return _container` ~ index.stringof ~ ";");
    }

    size_t _length = 0;
    size_t _capacity = 0;
    short _growthFactor = 2;

    void allocate(size_t newCapacity) @trusted
    {
        // if (_alloc is null)
        // {
        //     _alloc = allocatorObject(Mallocator.instance);
        // }
        import std.experimental.allocator : makeArray;
        static foreach (const index, _; S.tupleof)
        {
            getArray!index = PureMallocator.instance.makeArray!(Types[index])(newCapacity);
        }
    }

    void grow() @trusted
    {
        import std.algorithm.comparison : max;
        const newCapacity = max(1, _capacity * _growthFactor);
        const expandSize = newCapacity - _capacity;

        if (_capacity is 0)
        {
            allocate(newCapacity);
        }
        else
        {
            import std.experimental.allocator : expandArray;
            static foreach (const index, _; S.tupleof)
            {
                PureMallocator.instance.expandArray(getArray!index, expandSize);
            }
        }
        _capacity = newCapacity;
    }

    void reserveOneExtra() 
    {
        if (_length == _capacity) { grow(); }
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

    static if (isDIP1000)
    {
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
}

version(unittest)
{
    import dip_traits : isDIP1000;
}
