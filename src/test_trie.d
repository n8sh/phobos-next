#!/usr/bin/env rdmd

import std.algorithm;
import trie;
import dbg;

void main(string[] args)
{
    alias Key = long;
    auto set = radixTreeSet!(Key);

    const Key top = 1_000_000;
    foreach (i; 0 .. top)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
        assert(set.contains(i));
        assert(!set.insert(i));
        assert(set.contains(i));
    }

    foreach (i; 0 .. top)
    {
        assert(set.contains(i));
        assert(!set.insert(i));
    }

    size_t i = 0;
    foreach (const ref e; set[])
    {
        dln("e:", e, " i:", i);
        if (e != i)
        {
            assert(false, "Diff");
        }
        assert(e == i);
        ++i;
    }
}
