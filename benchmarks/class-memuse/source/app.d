/** Test memory usage and performance of struct and class construction.
 * https://dlang.org/spec/cpp_interface.html
 */

import std.stdio : write, writeln, writef, writefln;
import std.datetime : MonoTime;

extern(C++)
class NodeCxxClass
{
    this(ulong type)
    {
        this.type = type;
    }
    ulong type;
    EdgeCxxClass[] edges;
}

extern(C++)
class EdgeCxxClass
{
    this(ulong type)
    {
        this.type = type;
    }
    ulong type;
    // NodeCxxClass[] actors;
}

struct NodeCxxStruct
{
    this(ulong type)
    {
        this.type = type;
    }
    ulong type;
    EdgeCxxClass[] edges;
}

/// Show statistics.
void showStat(T)(const(char[]) typeName,
                 in T before,
                 in T after,
                 in size_t n)
{
    writefln("%s: %3.1f msecs (%3.1f ns/op)",
             typeName,
             cast(double)(after - before).total!"msecs",
             cast(double)(after - before).total!"nsecs" / n);
}

void main(string[] args)
{
    immutable n = 10_000_000;

    import basic_array : BasicArray;
    import std.array : Appender;

    {
        BasicArray!(NodeCxxStruct) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            x.put(NodeCxxStruct(42));
        }
        immutable after = MonoTime.currTime();

        showStat(typeof(x).stringof, before, after, n);
    }

    {
        Appender!(NodeCxxStruct[]) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            x.put(NodeCxxStruct(42));
        }
        immutable after = MonoTime.currTime();

        showStat(typeof(x).stringof, before, after, n);
    }

    {
        BasicArray!(NodeCxxClass) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            x.put(new NodeCxxClass(42));
        }
        immutable after = MonoTime.currTime();

        showStat(typeof(x).stringof, before, after, n);
    }

    {
        Appender!(NodeCxxClass[]) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            x.put(new NodeCxxClass(42));
        }
        immutable after = MonoTime.currTime();

        showStat(typeof(x).stringof, before, after, n);
    }

    import std.experimental.allocator : theAllocator, make;

    {
        BasicArray!(NodeCxxClass) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            x.put(theAllocator.make!NodeCxxClass(42));
        }
        immutable after = MonoTime.currTime();

        showStat(typeof(x).stringof ~ ".make", before, after, n);
    }

    {
        Appender!(NodeCxxClass[]) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (immutable i; 0 .. n)
        {
            x.put(theAllocator.make!NodeCxxClass(42));
        }
        immutable after = MonoTime.currTime();

        showStat(typeof(x).stringof ~ ".make", before, after, n);
    }
}
