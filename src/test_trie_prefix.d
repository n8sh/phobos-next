#!/usr/bin/env rdmd

// TODO uncomment test code at trie.d:4329 when this works
void main(string[] args)
{
    import trie : radixTreeSet;

    alias Key = string;
    auto set = radixTreeSet!(Key);

    set.insert("alpha");
    set.insert("alphabet");
    set.insert("a");
    set.insert("al");
    set.insert("all");

    import dbg;
    foreach (const e; set[])
    {
        dln(`"`, e, `"`);
    }
    import std.stdio;
    writeln();
    foreach (const e; set.prefix("a"))
    {
        dln(`"`, e, `"`);
    }

    import std.algorithm : equal;
    assert(set.prefix("a")
              .equal(["",
                      "l",
                      "ll",
                      "lpha",
                      "lphabet"]));

    set.print();
}
