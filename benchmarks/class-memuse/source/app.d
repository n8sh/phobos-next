extern(C++)
class NodeCxx
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    EdgeCxx[] edges;
}

extern(C++)
class EdgeCxx
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    NodeCxx[] actors;
}

void main(string[] args)
{
    import std.traits, std.meta, std.range, std.algorithm, std.stdio, std.array;
    Appender!(NodeCxx[]) as;

    immutable n = 10_000_000;
    as.reserve(n);

    foreach (i; 0 .. n)
    {
        as.put(new NodeCxx(42));
    }

    while (true)
    {
    }
}
