module nxt.fixed_appender;

/** Small-Size-Optimized (SSO) `Appender`.
 */
struct SSOAppender(T, size_t smallCapacity)
{
    import std.array : Appender;
    import fixed_array : FixedArray;

    void put(T x) @trusted
    {
        if (_isLarge)
            _large.put(x);
        else if (_small.full)
        {
            import std.algorithm.mutation : moveEmplaceAll;

            T[smallCapacity] tmp = void;
            moveEmplaceAll(_small[], tmp[0 .. _small.length]);

            import core.lifetime : emplace;
            emplace!Large(&_large);

            _large.put(tmp[]);
            _large.put(x);

            _isLarge = 1;
        }
        else
            _small.put(x);
    }

    inout(T)[] data() inout return scope
    {
        if (_isLarge)
            return _large.data[];
        else
            return _small[];
    }

private:
    alias Small = FixedArray!(T, smallCapacity);
    alias Large = Appender!(T[]);
    union
    {
        Small _small;
        Large _large;
    }
    bool _isLarge;
}

@safe pure nothrow unittest
{
    SSOAppender!(int, 2) a;
    a.put(11);
    a.put(12);
    assert(a.data[] == [11, 12]);
    a.put(13);
    assert(a.data[] == [11, 12, 13]);

    static if (isDIP1000)
    {
        static assert(!__traits(compiles, {
                    auto f() @safe pure
                    {
                        auto x = Str("alphas");
                        auto y = x[];
                        return y;   // errors with -dip1000
                    }
                }));
    }
}

version(unittest)
{
    import nxt.dip_traits : isDIP1000;
}
