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

    enum n = 200;
    foreach (const e; iota(0, n))
    {
        set.insert(e);
    }

    set.insert(n*2);
    foreach (const e; iota(n*3, n*3 + n))
    {
        set.insert(e);
    }

    // set.print();

    enum limit = n*2;
    // dln(set.upperBound(limit));
    // dln(set[].filter!(_ => _ > limit));

    assert(set.upperBound(limit)
           .equal(set[].filter!(_ => _ > limit)));
}
