#!/usr/bin/env rdmd

import std.algorithm, std.stdio;
import trie;

void main(string[] args)
{
    alias Key = int;

    auto set = radixTreeSet!(Key);

    set.insert(42);
    assert(set.contains(42));

    assert(set[].equal([42]));
}
