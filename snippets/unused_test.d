module unused_test;

import std.range;
import std.algorithm.iteration : map2 = map;
import std.algorithm.iteration : filter;
import std.stdio : writeln;
import io = std.stdio;

alias UsedInt = int;
alias PublicUnusedInt = int;
private alias PrivateUnusedInt = int;

int x = 42;
private int px = 42;

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

static void usedFunStatic()
{
}

void main()
{
    usedFun();
    usedFunStatic();
    enum e = isDynamicArray!(int);
    // auto i = isDynamicArray!(int);
}
