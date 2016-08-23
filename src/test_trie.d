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
        assert(set.insert(i));
        assert(!set.insert(i));
    }

    size_t i = 0;
    foreach (const e; set[])
    {
        if (e != i)
        {
            dln("e:", e, " i:", i);
        }
        assert(e == i);
        ++i;
    }
}
