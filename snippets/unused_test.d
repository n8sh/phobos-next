module unused_test;

import std.range.primitives;
import std.algorithm.iteration : map2 = map; // warn
import std.algorithm.iteration : filter;     // warn

import std.stdio : writeln;     // warn
public import std.stdio : write;

import io = std.stdio;

alias X = ElementType;

alias PublicUnusedInt = int;
private alias PrivateUnusedInt = int; // warn

alias PublicUsedInt = int;
private alias PrivateUsedInt = int;

version (D_LP64)
{
    private alias PrivateUInt = uint; // warn
    alias UInt = int;
}
else
{
    private alias PrivateUInt = uint;
    alias UInt = int;
}

int x = 42;
private int px = 42;            // warn

import std.traits : isDynamicArray, isStaticArray; // warn

void unusedFun()
{
labelA:                         // warn
    PublicUsedInt x;            // warn
    PrivateUsedInt y;           // warn
}

void usedFun()
{
    void usedNestedFun()
    {
        void unusedNestedFun()  // warn
        {
        }
    }
    usedNestedFun();
}

private void privateUnusedFun() // unused
{
    PublicUsedInt x;
    PrivateUsedInt y;           // unused
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
    int x;                      // unused
    // int y = xx;
    usedFun();
    privateUsedFun();
    usedFunStatic();
    enum e = isDynamicArray!(int);
    // auto i = isDynamicArray!(int);
    enum f = e;                 // unused
}

enum e = isDynamicArray!(int);
private enum f = isDynamicArray!(int); // unused

private struct S                // unused
{
    private void privateMember();
    public void publicMember();
}
public struct SP                // unused
{
    private void privateMember();
    public void publicMember();
}
private class C                 // unused
{
    private void privateMember();
    public void publicMember();
}
public class CP
{
    private void privateMember();
    public void publicMember();
}
private interface PIP            // unused
{
    private void privateMember();
    public void publicMember();
}
public interface IP
{
    private void privateMember();
    public void publicMember();
}
interface I             // unused
{
    private void privateMember();
    public void publicMember();
}

private struct TS(uint n_)      // unused
{
    alias n = n_;
}
private class TC(uint n_)       // unused
{
    alias n = n_;
}
private template T(uint n)      // unused
{
    alias N = n;
}
