module nxt.hybridsort;

import nxt.bijections : IntegralBijectableTypes;
import nxt.integer_sorting : radixSort;

static immutable size_t[IntegralBijectableTypes.length] radixSortMinLength;

shared static this()
{
    foreach (i, E; IntegralBijectableTypes)
    {
        import std.stdio : writeln;
        writeln("TODO Calculate radixSortMinLength for ", E.stringof);
        radixSortMinLength[i] = 0; // calulate limit
    }
}

import std.range.primitives : isRandomAccessRange;

auto hybridSort(alias less = "a < b", Range)(Range r)
    if (isRandomAccessRange!Range)
{
    import std.range.primitives : ElementType;
    import std.traits : isNumeric;
    static if (isNumeric!(ElementType!Range))
    {
        import nxt.integer_sorting : radixSort;
        return r.radixSort;
    }
    else
    {
        import std.algorithm.sorting : sort;
        return r.sort!less;
    }
}

unittest
{
    import std.meta : AliasSeq;

    const n = 1_000_000;

    foreach (ix, T; AliasSeq!(byte, short))
    {
        import std.container : Array;
        import std.algorithm : isSorted, swap;
        import nxt.random_ex : randInPlace;

        auto a = Array!T();
        a.length = n;

        a[].randInPlace();

        auto b = a.dup;

        a[].hybridSort;
        assert(a[].isSorted);

        import std.algorithm.sorting : sort;
        b[].sort;
        assert(b[].isSorted);

        assert(a == b);

        swap(a, b);
    }
}
