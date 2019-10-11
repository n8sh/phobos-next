#!/usr/bin/env rdmd-unittest-module

import std.traits, std.meta, std.range, std.algorithm, std.stdio;

// https://github.com/dlang/DIPs/blob/master/DIPs/accepted/DIP1018.md

struct A
{
    this(int x)
    {
        this.x = x;
    }
    this(ref return scope A rhs) // copy ctor
    {
        writeln("copying ", rhs.x);
        this.x = rhs.x;
    }
    int x;
}

void main()
{
    auto a = A(42);
    A b = a;            // calls copy constructor implicitly - prints "x"
    A c = A(b);         // calls constructor explicitly
    // immutable A d = a;  // calls copy constructor implicittly - prints 7
}
