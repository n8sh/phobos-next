#!/usr/bin/env rdmd

import std.algorithm.comparison : equal;
import trie : radixTreeSet;
import dbgio : dln;

void main(string[] args)
{
    struct S
    {
        byte byte_;
        short short_;
        int int_;
        long long_;
        float float_;
    }
    alias Key = S;
    auto set = radixTreeSet!(Key);
}
