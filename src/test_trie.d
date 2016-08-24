#!/usr/bin/env rdmd

import std.algorithm;
import trie;
import dbg;

// TODO uncomment test code at trie.d:4329 when this works
void main(string[] args)
{
    alias Key = ulong;
    auto set = radixTreeSet!(Key);

    const Key top = 256 + 1;    // TODO transformation from 256 to 256+1 is the problem
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

    enum span = 8;

    size_t i = 0;
    foreach (const ref key; set[])
    {
        const ok = key == i;

        KeyN!(span, Key.sizeof) ukey;
        const rawKey = key.toRawKey(ukey);

        if (!ok)
        {
            dln("Failed for rawKey:", rawKey, " key:", key, " i:", i, " ok:", ok);
            break;
        }
        assert(key == i);
        ++i;
    }

    set.print;
}
