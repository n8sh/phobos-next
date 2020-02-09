module nxt.filterarray;

import std.traits : isIntegral;

import nxt.filters : isDenseSetFilterable;

/** Container combining `DenseSetFilter` with growable array store.

    Has O(1) unordered element access via slicing.

    For use in graph algorithms with limited index ranges.

    TODO better name?
 */
struct DenseSetFilterGrowableArray(E,
                                   alias Allocator = null)
if (isDenseSetFilterable!E)
{
    import nxt.filters : DenseSetFilter, Growable, Copyable;
    import nxt.basic_array : DynamicArray;

    alias ElementType = E;

    @disable this(this);

    pragma(inline, true):

    /** Insert element `e`.
        Returns: precense status of element before insertion.
    */
    bool insert()(E e)          // template-lazy
    {
        const hit = _set.insert(e);
        if (!hit)
        {
            _array.insertBack(e);
        }
        return hit;
    }
    alias put = insert;         // OutputRange compatibility

    /// Check if element `e` is stored/contained.
    bool contains()(E e) const  // template-lazy
    {
        return _set.contains(e);
    }
    /// ditto
    bool opBinaryRight(string op)(E e) const
        if (op == "in")
    {
        return contains(e);
    }

    /// Check if empty.
    @property bool empty() const
    {
        return _array.empty;
    }

    /// Get length.
    @property size_t length() const
    {
        return _array.length;
    }

    /// Non-mutable slicing.
    auto opSlice() const        // template-lazy
    {
        return _array.opSlice;
    }

    /// Clear contents.
    void clear()()              // template-lazy
    {
        _set.clear();
        _array.clear();
    }

private:
    // TODO merge into store with only one length and capcity
    DenseSetFilter!(E, Growable.yes, Copyable.no) _set;
    DynamicArray!(E, Allocator) _array;
}

@safe pure nothrow @nogc:

unittest
{
    DenseSetFilterGrowableArray!uint x;

    assert(!x.insert(42));
    assert(x.contains(42));
    assert(x[] == [42].s);

    assert(x.insert(42));
    assert(x.contains(42));
    assert(x[] == [42].s);

    assert(!x.insert(43));
    assert(x.contains(43));
    assert(x[] == [42, 43].s);

    x.clear();
    assert(x.empty());

    assert(!x.insert(44));
    assert(x.contains(44));
    assert(x[] == [44].s);
}

version(unittest)
{
    import nxt.array_help : s;
}
