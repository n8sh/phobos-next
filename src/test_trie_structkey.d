#!/usr/bin/env rdmd

import std.algorithm.comparison : equal;
import trie : radixTreeSet;
import dbgio : dln;

void main(string[] args)
{
    struct S
    {
        byte byte_;
        short short_;
        int int_;
        long long_;
        float float_;
    }

    alias Key = S;

    auto set = radixTreeSet!(Key);
    assert(set.empty);

    const s = S.init;
    assert(!set.contains(s));
    assert(set.insert(s));
    assert(!set.insert(s));
    assert(set.contains(s));

    assert(!set.empty);
}
