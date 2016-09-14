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
    set.insert(3);
    set.insert(4);
    set.insert(5);
    const Key[2] expected = [4, 5];
    assert(set.upperBound(3)
              .equal(expected[]));
}
