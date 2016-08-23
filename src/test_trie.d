#!/usr/bin/env rdmd

import std.algorithm;
import trie;
import dbg;

void main(string[] args)
{
    alias Key = double;
    auto set = radixTreeSet!(Key);
    set.insert(4.2);
    set[].equal([4.2]);
}
