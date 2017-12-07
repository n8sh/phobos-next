#!/usr/bin/env rdmd

void main(string[] args)
{
    import std.algorithm.comparison : equal;
    import trie : RadixTreeSetGrowOnly;
    import dbgio : dln;
    import std.stdio : writeln;

    struct S
    {
        // string x;
        byte byte_;
        short short_;
        int int_;
        long long_;
        float float_;
        // string string_;
    }

    alias Key = S;
    RadixTreeSetGrowOnly!(Key) set;

    assert(set.empty);

    const n = 100;
    foreach (const byte i; 0 .. n)
    {
        const s = Key(i, i, i, i, i// , "i"
            );

        assert(!set.contains(s));
        assert(set.insert(s));

        assert(!set.insert(s));
        assert(set.contains(s));
    }

    // dln(set[]);
    assert(!set.empty);

    set.clear();
    assert(set.empty);
}
