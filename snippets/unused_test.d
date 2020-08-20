module unused_test;

import std.algorithm.iteration : mapx = map;
import std.algorithm.iteration : filter;

alias UsedInt = int;
alias UnusedInt = int;

import std.traits : isDynamicArray;

void unusedFun()
{
    UsedInt x;
}

void usedFun()
{
}

unittest
{
    usedFun();
    pragma(msg, isDynamicArray!(int));
}
