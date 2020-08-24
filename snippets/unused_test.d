module unused_test;

import std.range.primitives;
import std.algorithm.iteration : map2 = map;
import std.algorithm.iteration : filter;
import std.stdio : writeln;
import io = std.stdio;

alias X = ElementType;

alias PublicUnusedInt = int;
private alias PrivateUnusedInt = int;

alias PublicUsedInt = int;
private alias PrivateUsedInt = int;

version (D_LP64)
{
    private alias PrivateUInt = uint;
    alias UInt = int;
}
else
{
    private alias PrivateUInt = uint;
    alias UInt = int;
}

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

private struct TA(uint n_)
{
    alias n = n_;
}

private class TC(uint n_)
{
    alias n = n_;
}

private template T(uint n)
{
    alias N = n;
}
