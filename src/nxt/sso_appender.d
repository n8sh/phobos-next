module nxt.sso_appender;

/** Small-Size-Optimized (SSO) `Appender`.
 *
 * See_Also: https://forum.dlang.org/post/ifspcvfkwsnvyrdfngpw@forum.dlang.org
 */
struct SSOAppender(T, size_t smallCapacity)
if (smallCapacity >= 1)
{
    import std.array : Appender;
    import fixed_array : FixedArray;

    this(this)
    {
    }

    void assureOneMoreCapacity() @trusted
    {
        if (!_isLarge &&
            _small.full)
        {
            import std.algorithm.mutation : moveEmplaceAll;

            T[smallCapacity] tmp = void;
            moveEmplaceAll(_small[], tmp[0 .. _small.length]);

            import core.lifetime : emplace;
            emplace!Large(&_large);

            _large.put(tmp[]);
            _isLarge = 1;
        }
    }

    void put(T x) @trusted
    {
        import nxt.container_traits : needsMove;

        assureOneMoreCapacity();

        static if (needsMove!T)
            import core.lifetime : move;

        if (_isLarge)
        {
            static if (needsMove!T)
                _large.put(x.move);
            else
                _large.put(x);
        }
        else
        {
            static if (needsMove!T)
                _small.put(x.move);
            else
                _small.put(x);
        }
    }

    inout(T)[] data() inout return scope @trusted
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
    bool _isLarge;              // TODO: pack this into _small
}

@safe pure nothrow unittest
{
    alias A = SSOAppender!(int, 2);
    A a;
    a.put(11);
    a.put(12);
    assert(a.data[] == [11, 12]);
    a.put(13);
    assert(a.data[] == [11, 12, 13]);
    static if (isDIP1000)
    {
        auto f() @safe pure {
            A a;
            return a.data;   // errors with -dip1000
        }
        static assert(!__traits(compiles, {
                    auto f() @safe pure {
                        auto x = SmallAppender!(char)("alphas");
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
