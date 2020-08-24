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
    private void privateMember(); // unused
    public void publicMember(); // unused
}
public struct SP
{
    private void privateMember(); // unused
    public void publicMember();
}
private class C                 // unused
{
    private void privateMember(); // unused
    public void publicMember();   // unused
}
public class CP
{
    private void privateMember(); // unused
    public void publicMember();
}
private interface PIP            // unused
{
    private void privateMember(); // unused
    public void publicMember(); // unused
}
public interface IP
{
    private void privateMember(); // unused
    public void publicMember();
}
interface I
{
    private void privateMember(); // unused
    public void publicMember();
}

private class Base
{
    void member();              // used by `Derived.member()`
}
private class Derived : Base    // unused
{
    override void member()      // unused
    {
        super.member();
    }
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

void fun() @safe pure
{
    if (auto x = 3)             // TODO: unused
    {
    }
    if (const x = 3)            // TODO: unused
    {
    }
    if (immutable x = 3)        // TODO: unused
    {
    }
}

void fun() @safe pure
{
    foreach (const n; 0 .. 10)  // unused
    {
    }
}

/***********************************
 * Create a new associative array of the same size and copy the contents of the
 * associative array into it.
 * Params:
 *      aa =     The associative array.
 */
V[K] dup(T : V[K], K, V)(T aa)
{
    //pragma(msg, "K = ", K, ", V = ", V);

    // Bug10720 - check whether V is copyable
    static assert(is(typeof({ V v = aa[K.init]; })),
        "cannot call " ~ T.stringof ~ ".dup because " ~ V.stringof ~ " is not copyable");

    V[K] result;

    //foreach (k, ref v; aa)
    //    result[k] = v;  // Bug13701 - won't work if V is not mutable

    ref V duplicateElem(ref K k, ref const V v) @trusted pure nothrow
    {
        import core.stdc.string : memcpy;

        void* pv = _aaGetY(cast(AA*)&result, typeid(V[K]), V.sizeof, &k);
        memcpy(pv, &v, V.sizeof);
        return *cast(V*)pv;
    }

    static if (__traits(hasPostblit, V))
    {
        auto postblit = _getPostblit!V();
        foreach (k, ref v; aa)
            postblit(duplicateElem(k, v));
    }
    else
    {
        foreach (k, ref v; aa)
            duplicateElem(k, v);
    }

    return result;
}
