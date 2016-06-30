#!/usr/bin/env rdmd-dev-module

/** Extensions to std.algorithm.searching.
    Copyright: Per Nordlöw 2016-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
*/
module searching_ex;

/** This function returns the index of the `value` if it exist among `values`,
    `size_t.max` otherwise.

    TODO Should we extend to isRandomAccessRange support? In that case we don't
    get static array support by default.
*/
size_t binarySearch(R, E)(const R[] values, in E value)
    if (is(typeof(values[0].init == E.init))) // TODO SortedRange support
{
    // value is not in the array if the array is empty
    if (values.length == 0) { return typeof(return).max; }

    immutable mid = values.length / 2; // mid offset
    if (value == values[mid])
    {
        return mid; // direct hit
    }
    else if (value < values[mid])
    {
        return binarySearch(values[0 .. mid], value); // recurse left
    }
    else
    {
        const index = binarySearch(values[mid + 1 .. $], value); // recurse right
        if (index != typeof(return).max)
        {
            return index + mid + 1; // adjust the index; it is 0-based in the right-hand side slice.
        }
        return index;
    }
}

///
@safe pure nothrow @nogc unittest
{
    int[9] x = [1, 3, 5, 6, 8, 9, 10, 13, 15];
    assert(x.binarySearch(0) == size_t.max);
    assert(x.binarySearch(1) == 0);
    assert(x.binarySearch(2) == size_t.max);
    assert(x.binarySearch(3) == 1);
    assert(x.binarySearch(4) == size_t.max);
    assert(x.binarySearch(5) == 2);
    assert(x.binarySearch(6) == 3);
    assert(x.binarySearch(7) == size_t.max);
    assert(x.binarySearch(8) == 4);
    assert(x.binarySearch(9) == 5);
    assert(x.binarySearch(10) == 6);
    assert(x.binarySearch(11) == size_t.max);
    assert(x.binarySearch(12) == size_t.max);
    assert(x.binarySearch(13) == 7);
    assert(x.binarySearch(14) == size_t.max);
    assert(x.binarySearch(15) == 8);
}

import std.range : ElementType, SearchPolicy;

/** Index into `range` where element `e` either currently exists or should be
    inserted in order to preserve sortedness of the union of `range` and `value`.

    Returns:
    - `0`, if `e` should be place at the beginning
    - `range.length`, if `e` should be append at the end
      returned.

    Typically used by container modification/insertion algorithms.

    TODO Move to the member of `SortedRange` perhaps as a Voldemort return
    version of contains() or with an extra output index argument
*/
size_t sortedIndexOf(R, V, SearchPolicy sp = SearchPolicy.binarySearch)(R range, V value)
    if (is(typeof(ElementType!R.init == V.init))) // TODO SortedRange support
{
    return range.length - range.upperBound!sp(value).length; // always larger than zero
}

bool containsStoreIndex(R, V, SearchPolicy sp = SearchPolicy.binarySearch)(R range, V value, out size_t index)
    if (is(typeof(ElementType!R.init == V.init))) // TODO SortedRange support
{
    if (range.empty)
    {
        index = size_t.max;     // indicate undefined
        return false;           // no hit
    }

    index = range.length - range.upperBound!sp(value).length; // always larger than zero
    if (index == 0 && range[index] == value)
    {
        return true;
    }
    if (index >= 1 && range[index - 1] == value)
    {
        index = index - 1;
        return true;
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    int[2] x = [1, 3];
    size_t index;
    import std.range : assumeSorted;
    assert(!x[].assumeSorted.containsStoreIndex(0, index) && index == 0);
    assert( x[].assumeSorted.containsStoreIndex(1, index) && index == 0);
    assert(!x[].assumeSorted.containsStoreIndex(2, index) && index == 1);
    assert( x[].assumeSorted.containsStoreIndex(3, index) && index == 1);
    assert(!x[].assumeSorted.containsStoreIndex(4, index) && index == 2);
}
