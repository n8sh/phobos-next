module unused_test;

import std.range;
import std.algorithm.iteration : map2 = map;
import std.algorithm.iteration : filter;
import std.stdio : writeln;
import io = std.stdio;

alias PublicUnusedInt = int;
private alias PrivateUnusedInt = int;

alias PublicUsedInt = int;
private alias PrivateUsedInt = int;

int x = 42;
private int px = 42;

import std.traits : isDynamicArray, isStaticArray;

void unusedFun()
{
labelA:
    PublicUsedInt x;
    PrivateUsedInt y;
}

void usedFun()
{
    void f()
    {
        void g()
        {
        }
    }
}

private void privateUnusedFun()
{
    PublicUsedInt x;
    PrivateUsedInt y;
    x = x.init;
}

private void privateUsedFun()
{
}

static void usedFunStatic()
{
}

void main()
{
    int x;
    // int y = xx;
    usedFun();
    privateUsedFun();
    usedFunStatic();
    enum e = isDynamicArray!(int);
    // auto i = isDynamicArray!(int);
    enum f = e;
}

enum e = isDynamicArray!(int);
private enum f = isDynamicArray!(int);
