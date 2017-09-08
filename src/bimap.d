module bimap;

/** Bidirectional map between key-and-values of type `X` and `Y` inspired by C++
 * Boost Bimap (`boost::bimap`).

    See also: http://www.boost.org/doc/libs/1_65_1/libs/bimap/doc/html/boost_bimap/one_minute_tutorial.html
 */
struct BiMap(X, Y,
             alias Map = HashMap)
{
    alias LeftMap = Map!(X, Y);
    alias RightMap = Map!(Y, X);

    pragma(inline, true):

    void insert(X x,
                Y y)
    {
        _left[x] = y;
        _right[y] = x;
    }

    bool contains(in X x,
                  in Y y) const
    {
        // TODO do this symmetric?
        if (const hitPtr = x in _left)
        {
            return *hitPtr == y;
        }
        return false;
    }

    @safe pure nothrow @nogc:

    /// Check if empty.
    bool empty() const { return length == 0; }

    /// Get length.
    size_t length() const { return _left.length; }

    /// Non-mutating access to left map.
    ref const(LeftMap) left() const { return _left; }

    /// Non-mutating access to right map.
    ref const(RightMap) right() const { return _right; }

    LeftMap _left;
    RightMap _right;
}

private alias HashMap(Key, Value) = Value[Key];

@safe pure nothrow unittest
{
    BiMap!(size_t, string, HashMap) bm;

    bm.insert(42, "42");

    assert(bm.left.length == 1);
    assert(bm.right.length == 1);

    assert(bm.contains(42, "42"));
}
