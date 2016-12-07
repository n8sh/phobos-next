/** Extend std.algorithm.setopts to also operate on set- and map-like
    containers/ranges.

    See also: http://forum.dlang.org/post/nvd09v$24e9$1@digitalmars.com
*/
module setops_ex;

/** Specialization for `std.algorithm.setopts.setUnion` for AA. */
auto setUnionUpdate(T1, T2)(T1 a, T2 b)
    @trusted
    if (isAA!T1 &&
        isAA!T2)
{
    if (a.length < b.length)
    {
        return setUnionHelper(a, b);
    }
    else
    {
        return setUnionHelper(b, a);
    }
}

/** Helper function for `setUnionUpdate` that assumes `small` has shorter length than
    `large` .
*/
private static auto setUnionHelper(Small, Large)(const Small small, Large large)
{
    Large united = large.dup;   // TODO this shallow copy prevents large from being `const`
    foreach (const ref e; small.byKeyValue)
    {
        if (auto hitPtr = e.key in large)
        {
            (*hitPtr) = e.value; // TODO this potentially changes the value of
        }
        else
        {
            united[e.key] = e.value;
        }
    }
    return united;
}

/** Is `true` if `Set` is set-like container, that is provides membership
    checking via the `in` operator or `contains`.
    TODO Move to Phobos std.traits
*/
template hasContains(Set)
{
    import std.traits : hasMember;
    enum isSetOf = hasMember!(Set, "contains"); // TODO extend to check `in` operator aswell
}

/** Is `true` if `Map` is map-like container, that is provides membership
    checking via the `in` operator or `contains`.
    TODO Move to Phobos std.traits
*/
template isAA(Map)
{
    import std.traits : isAssociativeArray;
    enum isAA = isAssociativeArray!Map; // TODO check if in operator returns reference to value
}

version(unittest)
{
    import std.algorithm.comparison : equal;
    import dbgio : dln;
}

/// union of associative array (via keys)
@safe pure unittest
{
    alias Map = string[int];

    Map x = [0 : "a", 1 : "b"];
    Map y = [2 : "c"];

    Map c = [0 : "a", 1 : "b", 2 : "c"];

    // test associativity
    assert(setUnionUpdate(x, y) == c);
    assert(setUnionUpdate(y, x) == c);
}

import std.traits : CommonType;
import std.range.primitives;
import std.meta : allSatisfy, staticMap;
import std.functional : binaryFun;

struct SetIntersection2(alias less = "a < b", Rs...)
    if (Rs.length >= 2 && allSatisfy!(isInputRange, Rs) &&
        !is(CommonType!(staticMap!(ElementType, Rs)) == void))
{
private:
    Rs _input;
    alias comp = binaryFun!less;
    alias ElementType = CommonType!(staticMap!(.ElementType, Rs));

    // Positions to the first elements that are all equal
    void adjustPosition()
    {
        if (empty) return;

        size_t done = Rs.length;
        static if (Rs.length > 1) while (true)
        {
            foreach (i, ref r; _input)
            {
                alias next = _input[(i + 1) % Rs.length];

                if (comp(next.front, r.front))
                {
                    size_t popCount = 0;
                    do
                    {
                        next.popFront();
                        popCount += 1;
                        if (next.empty) return;
                    }
                    while (comp(next.front, r.front));
                    dln(popCount);
                    done = Rs.length;
                }
                if (--done == 0) return;
            }
        }
    }

public:
    ///
    this(Rs input)
    {
        this._input = input;
        // position to the first element
        adjustPosition();
    }

    ///
    @property bool empty()
    {
        foreach (ref r; _input)
        {
            if (r.empty) return true;
        }
        return false;
    }

    ///
    void popFront()
    {
        assert(!empty);
        static if (Rs.length > 1) foreach (i, ref r; _input)
        {
            alias next = _input[(i + 1) % Rs.length];
            assert(!comp(r.front, next.front));
        }

        foreach (ref r; _input)
        {
            r.popFront();
        }
        adjustPosition();
    }

    ///
    @property ElementType front()
    {
        assert(!empty);
        return _input[0].front;
    }

    static if (allSatisfy!(isForwardRange, Rs))
    {
        ///
        @property SetIntersection2 save()
        {
            auto ret = this;
            foreach (i, ref r; _input)
            {
                ret._input[i] = r.save;
            }
            return ret;
        }
    }
}

/// Ditto
SetIntersection2!(less, Rs) setIntersection2(alias less = "a < b", Rs...)(Rs ranges)
    if (Rs.length >= 2 && allSatisfy!(isInputRange, Rs) &&
        !is(CommonType!(staticMap!(ElementType, Rs)) == void))
{
    return typeof(return)(ranges);
}

@safe pure nothrow unittest
{
    auto si = setIntersection2([1, 2, 3],
                               [1, 2, 3]);
    const sic = si.save();
    assert(si.equal([1, 2, 3]));
}

unittest
{
    import std.algorithm.setops : setIntersection;
    import random_ex : randInPlaceWithElementRange;
    import array_ex : UncopyableArray;
    import algorithm_ex : collect;

    alias E = uint;
    alias A = UncopyableArray!E;

    immutable testLength = 10_000;
    immutable testCount = 10;
    E elementLow = 0;
    E elementHigh = 10_000_000;

    auto x = A.withLength(testLength);
    auto y = A.withLength(testLength);

    x[].randInPlaceWithElementRange(elementLow, elementHigh);
    y[].randInPlaceWithElementRange(elementLow, elementHigh);

    assert(setIntersection2(x[], y[])
           .equal(setIntersection(y[], x[])));

    void testSetIntersection()
    {
        auto z = setIntersection(x[], y[]).collect!A;
    }

    void testSetIntersection2()
    {
        auto z = setIntersection2(x[], y[]).collect!A;
    }

    import std.datetime : benchmark, Duration;
    auto r = benchmark!(testSetIntersection,
                        testSetIntersection2)(testCount);
    import std.stdio : writeln;
    import std.conv : to;
    writeln("old testSetIntersection: ", to!Duration(r[0]));
    writeln("new testSetIntersection: ", to!Duration(r[1]));

}
