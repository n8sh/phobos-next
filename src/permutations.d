#!/usr/bin/env rdmd-dev-module

module permutations;

import std.range.primitives: isInputRange;

/** Cartesian power.
   See_Also: http://forum.dlang.org/thread/mailman.1434.1339436657.24740.digitalmars-d-learn@puremagic.com#post-uftixibnvfffugjwsdbl:40forum.dlang.org
 */
struct CartesianPower(bool doCopy = true, T)
{
    T[] items;
    ulong repeat;
    T[] row;
    ulong i, maxN;

    this(T[] items_, in uint repeat_, T[] buffer)
    {
        this.items = items_;
        this.repeat = repeat_;
        row = buffer[0 .. repeat];
        row[] = items[0];
        maxN = items.length ^^ repeat;
    }

    static if (doCopy)
    {
        @property T[] front()
        {
            return row.dup;
        }
    }
    else
    {
        @property T[] front()
        {
            return row;
        }
    }

    @property bool empty() const @safe pure nothrow @nogc
    {
        return i >= maxN;
    }

    void popFront()
    {
        i++;
        if (empty) { return; }
        ulong n = i;
        size_t count = repeat - 1;
        while (n)
        {
            row[count] = items[n % items.length];
            count--;
            n /= items.length;
        }
    }
}

version(unittest)
{
    import array_help : s;
    static assert(isInputRange!(typeof([1, 2].s[].cartesianPower!false(4))));
}

auto cartesianPower(bool doCopy = true, T)(T[] items, in uint repeat)
    pure nothrow @safe
{
    return CartesianPower!(doCopy, T)(items, repeat, new T[repeat]);
}

auto cartesianPower(bool doCopy = true, T)(T[] items, in uint repeat, T[] buffer)
    pure nothrow @safe @nogc
{
    if (buffer.length >= repeat)
    {
        return CartesianPower!(doCopy, T)(items, repeat, buffer);
    }
    else
    {
        // Is this correct in presence of chaining?
        static immutable err = new Error("buffer.length < repeat");
        throw err;
    }
}

@safe pure nothrow @nogc unittest
{
    int[2] items = [1, 2];
    const n = 4;
    int[n] buf;
    import std.algorithm.comparison : equal;
    assert(items.cartesianPower!false(n, buf)
                .equal([[1, 1, 1, 1].s[],
                        [1, 1, 1, 2].s[],
                        [1, 1, 2, 1].s[],
                        [1, 1, 2, 2].s[],
                        [1, 2, 1, 1].s[],
                        [1, 2, 1, 2].s[],
                        [1, 2, 2, 1].s[],
                        [1, 2, 2, 2].s[],
                        [2, 1, 1, 1].s[],
                        [2, 1, 1, 2].s[],
                        [2, 1, 2, 1].s[],
                        [2, 1, 2, 2].s[],
                        [2, 2, 1, 1].s[],
                        [2, 2, 1, 2].s[],
                        [2, 2, 2, 1].s[],
                        [2, 2, 2, 2].s[]].s[]));
}
