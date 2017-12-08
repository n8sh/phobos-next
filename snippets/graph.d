@safe pure nothrow:

import std.array : Appender;

class Db
{
    Appender!(Node[]) nodes;
    Appender!(Edge[]) edges;
}

abstract class Zing
{
    @safe pure nothrow:
    abstract inout(Db) db() inout;           // get up-reference
}

class Node : Zing
{
    @safe pure nothrow:

    this(Db db) { _db = db; }

    pragma(inline, true)
    override final inout(Db) db() inout { return _db; }
    private Db _db;             // up-reference
}

class Edge : Zing
{
    @safe pure nothrow:

    this(Db db) { _db = db; }

    pragma(inline, true)
    override final inout(Db) db() inout { return _db; }
    private Db _db;             // up-reference
}

class Relation(uint arity) : Edge
    if (arity >= 2)
{
    @safe pure nothrow:
    this(Db db) { super(db); }
    Zing[arity] actors;
}

class Fn(uint arity) : Edge
    if (arity >= 1)
{
    @safe pure nothrow:
    this(Db db) { super(db); }
    Zing[arity] params;
}

@safe pure nothrow unittest
{
    Db db = new Db();
    Node node = new Node(db);
    Edge edge = new Edge(db);
    auto rel2 = new Relation!2(db);
    auto fn1 = new Fn!1(db);
}
