#!/usr/bin/env rdmd

import std.algorithm.comparison : equal;
import trie : radixTreeSet;
import dbgio : dln;
import std.stdio : writeln;

void main(string[] args)
{
    struct S
    {
        byte byte_;
        // short short_;
        // int int_;
        // long long_;
        // float float_;
        // string string_;
    }

    alias Key = S;

    auto set = radixTreeSet!(Key);
    assert(set.empty);

    const n = 100;
    foreach (const byte i; 0 .. n)
    {
        const s = Key(i//, i, i, i, i// , "i"
            );

        assert(!set.contains(s));
        assert(set.insert(s));

        assert(!set.insert(s));
        assert(set.contains(s));
    }

    assert(!set.empty);

    set.clear();
    assert(set.empty);
}
