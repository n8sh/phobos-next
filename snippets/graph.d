@safe pure nothrow:

import std.array : Appender;

class Db
{
    Appender!(Node[]) nodes;
    Appender!(Edge[]) edges;
}

/// Zing.
abstract class Zing
{
    @safe pure nothrow:
    abstract inout(Db) db() inout;           // get up-reference
}

/// Graph node.
class Node : Zing
{
    @safe pure nothrow:
    this(Db db)
    {
        _db = db;
    }

    pragma(inline, true)
    override final inout(Db) db() inout { return _db; }

    private Db _db;             // up-reference
}

/// Graph edge.
class Edge : Zing
{
    @safe pure nothrow:
    this(Db db)
    {
        _db = db;
    }

    pragma(inline, true)
    override final inout(Db) db() inout { return _db; }

    private Db _db;             // up-reference
}

@safe pure nothrow unittest
{
    Db db = new Db();
    Node node = new Node(db);
    Edge edge = new Edge(db);
}
