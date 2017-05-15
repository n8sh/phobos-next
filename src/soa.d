/** Structure of arrays.
    See also: https://maikklein.github.io/post/soa-d/
    See also: TODO Add my forum post
 */
module soa;

// @safe:

struct SOA(T)
    if (is(T == struct))        // TODO extend to `isAggregate!T`
{
    import std.experimental.allocator;
    import std.experimental.allocator.mallocator;

    import std.meta : staticMap;
    import std.traits : FieldNameTuple;

    alias toArray(T) = T[];
    alias toType(string s) = typeof(__traits(getMember, T, s));

    alias MemberNames = FieldNameTuple!T;
    alias Types = staticMap!(toType, MemberNames);
    alias ArrayTypes = staticMap!(toArray, Types);

    this(size_t size_, IAllocator _alloc = allocatorObject(Mallocator.instance))
    {
        alloc = _alloc;
        _capacity = size_;
        allocate(size_);
    }

    auto ref opDispatch(string name)()
    {
        import std.meta : staticIndexOf;
        alias index = staticIndexOf!(name, MemberNames);
        static assert(index >= 0);
        return containers[index];
    }

    void pushBack(Types types)
    {
        if (_length == _capacity) { grow(); }
        foreach (const index, ref container; containers)
        {
            container[_length] = types[index];
        }
        ++_length;
    }

    void pushBack(T t)
    {
        if (_length == _capacity) { grow(); }
        foreach (const index, _; Types)
        {
            containers[index][_length] = __traits(getMember, t, MemberNames[index]);
        }
        ++_length;
    }

    size_t length() const @property
    {
        return _length;
    }

    ~this()
    {
        if (alloc is null) { return; }
        foreach (ref container; containers)
        {
            alloc.dispose(container);
        }
    }

private:

    void length(size_t newLength) @property
    {
        _length = newLength;
    }

    import std.typecons : Tuple;
    Tuple!ArrayTypes containers;

    IAllocator alloc;

    size_t _length = 0;
    size_t _capacity = 0;
    short growFactor = 2;

    void allocate(size_t size_)
    {
        if (alloc is null)
        {
            alloc = allocatorObject(Mallocator.instance);
        }
        foreach (const index, ref container; containers)
        {
            container = alloc.makeArray!(Types[index])(size_);
        }
    }

    void grow()
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
            foreach (ref container; containers)
            {
                alloc.expandArray(container, expandSize);
            }
        }
        _capacity = newCapacity;
    }
}

version(unittest)
{
    import dbgio;
}

unittest
{
    struct S { int i; float f; }

    auto x = SOA!S();
    assert(x.length == 0);
    x.pushBack(S.init);
    assert(x.length == 1);

    auto x3 = SOA!S(3);
    assert(x3.length == 0);
}
