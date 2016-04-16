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
    static if (isIntegral!(ElementType!Range))
    {
        import intsort : radixSort;
        r.radixSort;
        import std.algorithm.sorting : assumeSorted;
        return r.assumeSorted;
    }
    else
    {
        import std.algorithm.sorting : sort;
        return sort!less(r);
    }
}
