extern(C++)
class Node
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    Edge[] edges;
}

extern(C++)
class Edge
{
    this(ubyte type)
    {
        this.type = type;
    }
    ulong type;
    Node[] actors;
}

void main(string[] args)
{
    import std.traits, std.meta, std.range, std.algorithm, std.stdio, std.array;
    Appender!(Node[]) as;

    const n = 10_000_000;
    as.reserve(n);

    foreach (i; 0 .. n)
    {
        as.put(new Node(42));
    }

    while (true)
    {
    }
}
