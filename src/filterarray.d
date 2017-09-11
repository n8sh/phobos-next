module filterarray;

import std.traits : isIntegral;

import filters : isDenseSetFilterable;

/** Container that combines `DenseSetFilter` with O(1) unordered element access via
    slicing but not indexing for now.
 */
struct DenseSetFilterGrowableArray(E)
    if (isDenseSetFilterable!E)
{
    import filters : DenseSetFilter, Growable, Copyable;
    import array_ex : UniqueArray;

    @disable this(this);

    pragma(inline, true):

    /** Insert element `e`.
        Returns: precense status of element before insertion.
    */
    bool insert(in E e)
    {
        const hit = _set.insert(e);
        if (!hit)
        {
            _array.put(e);
        }
        return hit;
    }
    alias put = insert;         // OutputRange compatibility

    /// Check if element `e` is stored/contained.
    bool contains(in E e) const
    {
        return _set.contains(e);
    }

    /// Check if empty.
    bool empty() const
    {
        return _array.empty;
    }

    /// Non-mutable slicing.
    auto opSlice() const
    {
        return _array.opSlice;
    }

    void clear()
    {
        _set.clear();
        _array.clear();
    }

private:
    DenseSetFilter!(E, Growable.yes, Copyable.no) _set;
    UniqueArray!(E) _array;     // non-copyable
}

@safe pure nothrow @nogc:

unittest
{
    DenseSetFilterGrowableArray!uint x;

    assert(!x.insert(42));
    assert(x.contains(42));
    assert(x[] == [42].s[]);

    assert(x.insert(42));
    assert(x.contains(42));
    assert(x[] == [42].s[]);

    assert(!x.insert(43));
    assert(x.contains(43));
    assert(x[] == [42, 43].s[]);

    x.clear();
    assert(x.empty());

    assert(!x.insert(44));
    assert(x.contains(44));
    assert(x[] == [44].s[]);
}

version(unittest)
{
    import array_ex : s;
}
