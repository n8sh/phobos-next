import std.stdio : write, writeln, writef, writefln;
import std.datetime : MonoTime;

extern(C++)
class NodeCxx
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    // EdgeCxx[] edges;
}

extern(C++)
class EdgeCxx
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    // NodeCxx[] actors;
}

void main(string[] args)
{
    immutable n = 10_000_000;

    {
        import std.array : Appender;
        Appender!(NodeCxx[]) x;

        x.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (i; 0 .. n)
        {
            x.put(new NodeCxx(42));
        }
        immutable after = MonoTime.currTime();

        writefln("Appender: %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
    }

    {
        import basic_array : BasicArray;
        BasicArray!(NodeCxx) y;

        y.reserve(n);

        immutable before = MonoTime.currTime();
        foreach (i; 0 .. n)
        {
            y.insertBack(new NodeCxx(42));
        }
        immutable after = MonoTime.currTime();

        writefln("BasicArray: %3.1f ns/op", cast(double)(after - before).total!"nsecs" / n);
    }
}
