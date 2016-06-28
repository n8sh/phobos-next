#!/usr/bin/env rdmd-dev-module

/** Extensions to std.algorithm.searching.
    Copyright: Per Nordlöw 2014-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
*/
module searching_ex;

/** This function returns the index of the `value` if it exist among `values`,
    `size_t.max` otherwise.
*/
size_t binarySearch(T, U)(const T[] values, in U value)
    if (is(typeof(values[0].init == U.init))) // TODO SortedRange support
{
    // value is not in the array if the array is empty
    if (values.length == 0) { return size_t.max; }

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
        auto index = binarySearch(values[mid + 1 .. $], value); // recurse right
        if (index != size_t.max)
        {
            index += mid + 1; // adjust the index; it is 0-based in the right-hand side slice.
        }
        return index;
    }
}

@safe pure nothrow @nogc unittest
{
    int[9] x = [1, 3, 5, 6, 8, 9, 10, 13, 15];

    // check hit
    assert(x[].binarySearch(1) != size_t.max);
    assert(x[].binarySearch(13) != size_t.max);

    // check miss
    assert(x[].binarySearch(0) == size_t.max);
    assert(x[].binarySearch(14) == size_t.max);
}
