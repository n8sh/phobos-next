#!/usr/bin/env rdmd

import std.algorithm;
import trie;
import dbg;

void main(string[] args)
{
    alias Key = float;
    auto set = radixTreeSet!(Key);
    set.insert(4.2f);
    set[].equal([4.2f]);
}
