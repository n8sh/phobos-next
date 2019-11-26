#!/usr/bin/env rdmd

import std.algorithm.comparison : equal;
import nxt.dbgio : dbg;
import std.stdio : writeln;
import nxt.trie : RadixTreeSetGrowOnly;

alias Tree = RadixTreeSetGrowOnly!string;

// void recursiveTest(Tree visits = Tree.init, size_t maxCount = 0)
// {
//     recursiveTest(visits, ++maxCount);
// }

void main(string[] args)
{
    Tree t;
    dbg("1");
    auto t2 = t.dup;
    dbg("2");
    // recursiveTest();
}
