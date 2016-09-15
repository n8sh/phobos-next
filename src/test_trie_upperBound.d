#!/usr/bin/env rdmd

void main()
{
    import std.algorithm.comparison : equal;
    import std.range : iota;
    import trie : radixTreeSet;
    import dbgio : dln;

    alias Key = int;
    auto set = radixTreeSet!Key;

    set.clear();

    auto expected = iota(0, 10);
    foreach (const e; expected)
    {
        set.insert(e);
    }

    const limit = 3;
    dln(set.upperBound(limit));

    assert(set.upperBound(limit)
           .equal(iota(limit + 4, 10)));
}
