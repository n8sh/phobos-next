#!/usr/bin/env dub
/+ dub.sdl:
    name "example"
    dependency "automem" version="~>0.3.3"
    dependency "test_allocator" version="~>0.2.1"
+/
module app;

import std.experimental.allocator;
import std.stdio;
import std.typecons;

import automem;
import test_allocator;

alias Tup = Tuple!(int, int);

struct A(Allocator)
{
    UniqueArray!(int, Allocator) data;
    alias data this;

    this(Allocator alloc)
    {
        data = typeof(data)(alloc);
    }

    // This is how I would like to implement the move (kind of).
    // Is there a better way?
    this(typeof(data) other)
    {
        this.data = other.move;
    }

    auto move()
    {
        return typeof(this)(data.move);
    }
}

// My problem is that I had to use this construction to make AA work.
// This in turn lead to e.g. length segfaulting after "move".
// Which lead to it being very, very easy to introduce bugs in the software.
struct Aw(Allocator)
{
    alias ArrT = UniqueArray!(Tup, Allocator);
    RefCounted!(ArrT, Allocator) data;

    ref auto get() return inout
    {
        return *data;
    }

    alias get this;

    this(Allocator alloc)
    {
        data = typeof(data)(alloc, alloc);
    }

    this(RefCounted!(ArrT, Allocator) d)
    {
        data = d;
    }

    // How to implement a "move" that is easy to use correctly?
    // This is not correct... wrong type.
    // Unable to instantiate Aw without either `move` taking an allocator
    // parameter or storing an allocator in Aw.
    ArrT move()
    {
        return (*data).move;
    }

    // this lead to segfaults....
    auto move2()
    {
        auto tmp = data;
        // probably not a good idea.. wrong usage. I want to release the
        // RefCounted so this instance and the returned do not refer to the
        // same underlying data.
        destroy(data);
        return typeof(this)(tmp);
    }
}

struct AA(T, Allocator)
{
    UniqueArray!(T!Allocator, Allocator) data;
    alias data this;

    this(Allocator alloc)
    {
        data = typeof(data)(alloc);
    }
}

void main(string[] args)
{
    TestAllocator talloc;
    alias Alloc = typeof(&talloc);

    auto a = A!Alloc(&talloc);

    // this do not work
    //auto aa = AA!(A!Alloc, Alloc)(&talloc);

    // but this do
    auto aa = AA!(Aw!Alloc, Alloc)(&talloc);

    // some examples of how to use
    writeln("aa len: ", aa.length); // 0
    aa ~= Aw!Alloc(&talloc);
    aa ~= Aw!Alloc(&talloc);
    writeln("aa len: ", aa.length); // 2
    aa[0] ~= Tup(1, 2);
    aa[1] ~= Tup(3, 2);
    writeln(aa[0][]); // [2]

    // lets "move" an object
    auto moved_a = aa[1].move; // but moved_a isn't an Aw type anymore :/
    writeln("aa len: ", aa.length); // 2
    writeln("moved_a len: ", moved_a.length);

    // shrink to remove the moved element
    aa.length = 1;
    writeln("aa[1] len: ", aa[1].length);
    writeln("moved_a len: ", moved_a.length);

    // lets see what happens with move2
    auto move2_a = aa[0].move2; // nice the type is Aw! as expected
    writeln("move2_a len: ", move2_a.length);
    // this will segfault. this is why it is so error prone to do it this way.
    writeln("aa[0] len: ", aa[0].length);
}
