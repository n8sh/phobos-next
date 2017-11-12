/** Test memory usage and performance of struct and class construction.
 * https://dlang.org/spec/cpp_interface.html
 */

import std.stdio : write, writeln, writef, writefln;
import std.datetime : MonoTime;

extern(C++)
class NodeCxxClass
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    EdgeCxxClass[] edges;
}

extern(C++)
class EdgeCxxClass
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    // NodeCxxClass[] actors;
}

struct NodeCxxStruct
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    EdgeCxxClass[] edges;
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
        foreach (i; 0 .. n)
        {
            x.put(NodeCxxStruct(42));
        }
        immutable after = MonoTime.currTime();

        writefln("%s: %3.1f msecs (%3.1f ns/op)",
                 typeof(x).stringof,
                 cast(double)(after - before).total!"msecs",
                 cast(double)(after - before).total!"nsecs" / n);
    }

    {
        Appender!(NodeCxxStruct[]) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (i; 0 .. n)
        {
            x.put(NodeCxxStruct(42));
        }
        immutable after = MonoTime.currTime();

        writefln("%s: %3.1f msecs (%3.1f ns/op)",
                 typeof(x).stringof,
                 cast(double)(after - before).total!"msecs",
                 cast(double)(after - before).total!"nsecs" / n);
    }

    {
        BasicArray!(NodeCxxClass) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (i; 0 .. n)
        {
            x.put(new NodeCxxClass(42));
        }
        immutable after = MonoTime.currTime();

        writefln("%s: %3.1f msecs (%3.1f ns/op)",
                 typeof(x).stringof,
                 cast(double)(after - before).total!"msecs",
                 cast(double)(after - before).total!"nsecs" / n);
    }

    {
        Appender!(NodeCxxClass[]) x;
        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (i; 0 .. n)
        {
            x.put(new NodeCxxClass(42));
        }
        immutable after = MonoTime.currTime();

        writefln("%s: %3.1f msecs (%3.1f ns/op)",
                 typeof(x).stringof,
                 cast(double)(after - before).total!"msecs",
                 cast(double)(after - before).total!"nsecs" / n);
    }
}
