module bimap;

/** Bidirectional map between key-and-values of type `KeyA` and `KeyB`.
 */
struct BiMap(KeyA, KeyB,
             alias Map = HashMap)
{
    alias LeftMap = Map!(KeyA, KeyB);
    alias RightMap = Map!(KeyB, KeyA);

    pragma(inline, true):

    void insert(KeyA a,
                KeyB b)
    {
        _left[a] = b;
        _right[b] = a;
    }

    bool contains(in KeyA a,
                  in KeyB b) const
    {
        // TODO do this symmetric?
        if (const hitPtr = a in _left)
        {
            return *hitPtr == b;
        }
        return false;
    }

    @safe pure nothrow @nogc:

    /// Check if empty.
    bool empty() const { return length == 0; }

    /// Get length.
    size_t length() const { return _left.length; }

    /// Non-mutating access.
    ref const(LeftMap) left() const { return _left; }

    /// Non-mutating access.
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
