/** Structure of arrays similar to builtin feature in the Jai programming language.
 *
 * See_Also: https://maikklein.github.io/post/soa-d/
 * See_Also: http://forum.dlang.org/post/wvulryummkqtskiwrusb@forum.dlang.org
 * See_Also: https://forum.dlang.org/post/purhollnapramxczmcka@forum.dlang.org
 */
module nxt.soa;

/** Structure of arrays similar to members of `S`.
 */
struct SOA(S)
if (is(S == struct))        // TODO extend to `isAggregate!S`?
{
    import core.lifetime : move, moveEmplace;
    import nxt.pure_mallocator : PureMallocator;

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
    void insertBack()(S value) @trusted // template-lazy
    {
        reserveOneExtra();
        static foreach (const index, memberSymbol; S.tupleof)
        {
            moveEmplace(__traits(getMember, value, memberSymbol.stringof),
                        getArray!index[_length]); // TODO assert that
        }
        ++_length;
    }

    /// Push element (struct) `value` to back of array using its data members `members`.
    void insertBackMembers()(Types members) @trusted // template-lazy
    {
        reserveOneExtra();
        // move each member to its position respective array
        static foreach (const index, _; members)
        {
            moveEmplace(members[index], getArray!index[_length]); // same as `getArray!index[_length] = members[index];`
        }
        ++_length;
    }

    void opOpAssign(string op, S)(S value)
        if (op == "~")
    {
        pragma(inline, true);
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

    ~this() @trusted @nogc
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

    /** Slice operator. */
    inout(SOASlice!S) opSlice()() inout return // template-lazy
    {
        return typeof(return)(&this);
    }

private:

    // generate array definitions
    static foreach (index, Type; Types)
    {
        mixin(Type.stringof ~ `[] _container` ~ index.stringof ~ ";");
    }

    /// Get array of all fields at aggregate field index `index`.
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
alias StructArrays = SOA;

/// Reference to element in `soaPtr` at index `elementIndex`.
private struct SOAElementRef(S)
if (is(S == struct))        // TODO extend to `isAggregate!S`?
{
    SOA!S* soaPtr;
    size_t elementIndex;

    @disable this(this);

    /// Access member name `memberName`.
    auto ref opDispatch(string memberName)()
        @trusted return scope
    {
        mixin(`return ` ~ `(*soaPtr).` ~ memberName ~ `[elementIndex];`);
    }
}

/// Reference to slice in `soaPtr`.
private struct SOASlice(S)
    if (is(S == struct))        // TODO extend to `isAggregate!S`?
{
    SOA!S* soaPtr;

    @disable this(this);

    /// Access aggregate at `index`.
    inout(S) opIndex(size_t index) inout @trusted return scope
    {
        S s = void;
        static foreach (memberIndex, memberSymbol; S.tupleof)
        {
            mixin(`s.` ~ memberSymbol.stringof ~ `= (*soaPtr).getArray!` ~ memberIndex.stringof ~ `[index];`);
        }
        return s;
    }
}

@safe:

@safe pure nothrow @nogc unittest
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

    // TODO make foreach work
    // foreach (_; x[])
    // {
    // }

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
    import nxt.dip_traits : isDIP1000;
}
