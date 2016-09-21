#!/usr/bin/env rdmd

import std.algorithm.comparison : equal;
import dbgio : dln;
import std.stdio : writeln;
import trie : RadixTreeSetGrowOnly;

alias Tree = RadixTreeSetGrowOnly!string;

// void recursiveTest(Tree visits = Tree.init, size_t maxCount = 0)
// {
//     recursiveTest(visits, ++maxCount);
// }

void main(string[] args)
{
    Tree t;
    dln("1");
    auto t2 = t.dup;
    dln("2");
    // recursiveTest();
}
