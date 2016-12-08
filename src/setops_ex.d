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

// version = show;

import std.traits : CommonType;
import std.range.primitives;
import std.meta : allSatisfy, staticMap;
import std.functional : binaryFun;
import std.algorithm.sorting : SearchPolicy;

struct SetIntersection2(alias less = "a < b",
                        SearchPolicy preferredSearchPolicy = SearchPolicy.gallop,
                        Rs...)
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

        size_t compsLeft = Rs.length; // number of compares left
        static if (Rs.length > 1) while (true)
        {
            foreach (i, ref r; _input)
            {
                alias next = _input[(i + 1) % Rs.length]; // requires copying of range

                // TODO Use upperBound only when next.length / r.length > 12

                import std.range : isRandomAccessRange;
                static if (allSatisfy!(isRandomAccessRange, typeof(next)))
                {
                    import std.algorithm.sorting : assumeSorted;
                    version (show) dln("next:", next);
                    version (show) dln("r.front:", r.front);

                    // TODO remove need for this hack
                    static if (less == "a < b")
                    {
                        enum lessEq = "a <= b";
                    }
                    else static if (less == "a > b")
                    {
                        enum lessEq = "a >= b";
                    }

                    // TODO can we merge thsse two lines two one single assignment from nextUpperBound to next
                    auto nextUpperBound = next.assumeSorted!lessEq.upperBound!preferredSearchPolicy(r.front);
                    next = next[$ - nextUpperBound.length .. $];

                    version (show) dln("nextUpperBound:", nextUpperBound);

                    if (next.empty)
                    {
                        return; // next became empty, so everything becomes empty
                    }
                    else if (next.front != r.front)
                    {
                        compsLeft = Rs.length; // we need to start counting comparing again starting with next.front
                    }
                }
                else
                {
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
                        compsLeft = Rs.length;
                    }
                }
                if (--compsLeft == 0) return; // count down, and if we have made Rs.length iterations we are compsLeft finding a common front element
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
SetIntersection2!(less, preferredSearchPolicy, Rs) setIntersection2(alias less = "a < b",
                                                           SearchPolicy preferredSearchPolicy = SearchPolicy.gallop,
                                                           Rs...)(Rs ranges)
    if (Rs.length >= 2 && allSatisfy!(isInputRange, Rs) &&
        !is(CommonType!(staticMap!(ElementType, Rs)) == void))
{
    foreach (r; ranges)
    {
        version(show) dln("r:", r);
    }
    return typeof(return)(ranges);
}

unittest
{
    import std.algorithm.sorting : sort;
    import std.algorithm.setops : setIntersection;
    import random_ex : randInPlaceWithElementRange;
    import array_ex : UncopyableArray;
    import algorithm_ex : collect;

    alias E = ulong;
    alias A = UncopyableArray!E;

    auto a0 = A();
    auto a1 = A.withElements(1);

    enum less = "a < b";

    auto s0 = setIntersection2!(less)(a0[], a0[]);
    assert(s0.equal(a0[]));

    auto s1 = setIntersection2!(less)(a1[], a1[]);
    assert(s1.equal(a1[]));

    immutable smallTestLength = 1000;
    immutable factor = 12; // this is the magical limit on my laptop when performance of upperBound beats standard implementation
    immutable largeTestLength = factor*smallTestLength;
    E elementLow = 0;
    E elementHigh = 10_000_000;
    auto x = A.withLength(smallTestLength);
    auto y = A.withLength(largeTestLength);

    x[].randInPlaceWithElementRange(elementLow, elementHigh);
    y[].randInPlaceWithElementRange(elementLow, elementHigh);

    sort(x[]);
    sort(y[]);

    // associative
    assert(equal(setIntersection2!(less)(x[], y[]),
                 setIntersection2!(less)(y[], x[])));

    // same as current
    assert(equal(setIntersection!(less)(x[], y[]),
                 setIntersection2!(less)(x[], y[])));

    void testSetIntersection()
    {
        auto z = setIntersection!(less)(x[], y[]).collect!A;
    }

    void testSetIntersectionNew()
    {
        auto z = setIntersection2!(less)(x[], y[]).collect!A;
    }

    import std.datetime : benchmark, Duration;
    immutable testCount = 10;
    auto r = benchmark!(testSetIntersection,
                        testSetIntersectionNew)(testCount);
    import std.stdio : writeln;
    import std.conv : to;
    writeln("old testSetIntersection: ", to!Duration(r[0]));
    writeln("new testSetIntersection: ", to!Duration(r[1]));

}

@safe pure nothrow unittest
{
    enum less = "a < b";
    auto si = setIntersection2!(less)([1, 2, 3],
                                      [1, 2, 3]);
    const sic = si.save();
    assert(si.equal([1, 2, 3]));
}
