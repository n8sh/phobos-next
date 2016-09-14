#!/usr/bin/env rdmd

void main(string[] args)
{
    import std.algorithm.comparison : equal;
    import trie : radixTreeSet;
    import dbgio : dln;

    alias Key = int;
    auto set = radixTreeSet!(Key);

    set.clear();

    set.insert(1);
    set.insert(2);
    set.insert(3);              // limit
    set.insert(4);
    set.insert(5);
    set.insert(6);
    set.insert(7);

    const Key[4] expected = [4, 5, 6, 7];
    assert(set.upperBound(3)
              .equal(expected[]));
}
