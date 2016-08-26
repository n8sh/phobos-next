#!/usr/bin/env rdmd

import trie;

// TODO uncomment test code at trie.d:4329 when this works
void main(string[] args)
{
    alias Key = string;
    auto set = radixTreeSet!(Key);

    set.insert("alpha");
    set.insert("alphabeth");
    set.insert("a");
    set.insert("al");

    import dbg;
    foreach (const e; set.prefix("a"))
    {
        dln(e);
    }

    // import std.algorithm : equal;
    // assert(set.prefix("a").equal(["", "l", "lpha", "lphabeth"]));

    set.print();
}
