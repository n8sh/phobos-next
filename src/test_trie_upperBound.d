#!/usr/bin/env rdmd

void main()
{
    import std.algorithm.comparison : equal;
    import std.range : iota;
    import std.algorithm : filter;
    import trie : radixTreeSet;
    import dbgio : dln;

    alias Key = int;
    auto set = radixTreeSet!Key;

    set.clear();

    enum n = 29;
    foreach (const e; iota(0, n))
    {
        set.insert(e);
    }
    set.insert(n*2);

    enum limit = 3;
    dln(set.upperBound(limit));
    dln(set[].filter!(_ => _ > limit));

    assert(set.upperBound(limit)
           .equal(set[].filter!(_ => _ > limit)));
}
