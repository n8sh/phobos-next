module nxt.bimap;

@safe pure:

/** Bidirectional map between key-and-values of type `X` and `Y` inspired by C++
    Boost Bimap (`boost::bimap`).

    See_Also: http://www.boost.org/doc/libs/1_65_1/libs/bimap/doc/html/boost_bimap/one_minute_tutorial.html
 */
struct BiMap(X, Y,
             alias Map = Y[X])
{
@safe pure nothrow:
    alias LeftMap = Map!(X, Y);
    alias RightMap = Map!(Y, X);

    /// Insert (`x`, `y`).
    void insert(X x, Y y)
    {
        version(D_Coverage) {} else pragma(inline, true);
        _left[x] = y;
        _right[y] = x;
    }

    /// Check if (`x`, `y`) is stored.
    bool contains(scope const X x,
                  scope const Y y) const
    {
        version(LDC) pragma(inline, true);
        // TODO do this symmetric?
        if (const hitPtr = x in _left)
        {
            return *hitPtr == y;
        }
        return false;
    }

    /// Clear contents.
    void clear() @trusted       // TODO ok for this to be `@trusted`?
    {
        version(D_Coverage) {} else pragma(inline, true);
        _left.clear();
        _right.clear();
    }

@nogc:

    /// Check if empty.
    bool empty() const
    {
        version(D_Coverage) {} else pragma(inline, true);
        return length == 0;
    }

    /// Get length.
    size_t length() const
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _left.length;
    }

    /// Access to left map must be non-mutating.
    ref const(LeftMap) left() const
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _left;
    }

    /// Access to right map must be non-mutating.
    ref const(RightMap) right() const
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _right;
    }

private:
    LeftMap _left;              ///< Left map.
    RightMap _right;            ///< Right map.
}

@safe pure nothrow:

/// test with builtin associative arrays
unittest
{
    alias HashMap(Key, Value) = Value[Key];
    BiMap!(size_t, string, HashMap) bm;

    bm.insert(42, "42");

    assert(bm.left.length == 1);
    assert(bm.right.length == 1);

    assert(bm.contains(42, "42"));

    bm.clear();
    assert(bm.empty);
}
