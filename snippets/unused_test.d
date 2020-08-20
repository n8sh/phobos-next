module unused_test;

import std.range;
import std.algorithm.iteration : map2 = map;
import std.algorithm.iteration : filter;

alias UsedInt = int;
alias UnusedInt = int;

import std.traits : isDynamicArray;

void unusedFun()
{
    UsedInt x;
}

private void privateUnusedFun()
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
