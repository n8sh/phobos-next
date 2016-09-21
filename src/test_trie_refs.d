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
    auto t2 = t;
    auto t3 = t2.dup;
    // recursiveTest();
}
