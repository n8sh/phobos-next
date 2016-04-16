module hybridsort;

static immutable radixSortMinLength = 0;

shared static this()
{
    // calculate radixSortMinLength
}

auto hybridSort(alias less = "a < b", Range)(Range r)
    if (isRandomAccessRange!Range)
{
    import std.range : ElementType;
    static if (isNumeric!(ElementType!Range))
    {
        import intsort : radixSort;
        return r.radixSort;
    }
    else
    {
        import std.algorithm.sorting : sort;
        return r.sort!less;
    }
}
