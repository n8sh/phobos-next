#!/usr/bin/env rdmd

import std.algorithm;
import trie;

// TODO uncomment test code at trie.d:4329 when this works
void main(string[] args)
{
    alias Key = ubyte;
    auto set = radixTreeSet!(Key);

    const size_t top = 256;
    assert(!set._root);

    foreach (const i; 0 .. 7)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
    }

    foreach (const i; 7 .. 48)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
    }

    foreach (const i; 48 .. 256)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
    }

    set.print;

    enum span = 8;

    size_t i = 0;
    foreach (const ref key; set[])
    {
        const ok = key == i;
        KeyN!(span, Key.sizeof) ukey;
        const rawKey = key.toRawKey(ukey);
        // if (!ok) { dln("Failed for rawKey:", rawKey, " key:", key, " i:", i, " ok:", ok); break; }
        assert(key == i);
        ++i;
    }
}
