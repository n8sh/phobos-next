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

    this(Db db)
    {
        _db = db;
        db.nodes.put(this);
    }

    pragma(inline, true)
    override final inout(Db) db() inout { return _db; }
    private Db _db;             // up-reference
}

class Text : Node
{
    @safe pure nothrow:
    this(Db db, string text)
    {
        super(db);
        this.text = text;
    }
    const string text;
}

class Number(T) : Node
{
    @safe pure nothrow:
    this(Db db, T value)
    {
        super(db);
        this.value = value;
    }
    const T value;
}

class Edge : Zing
{
    @safe pure nothrow:

    this(Db db)
    {
        _db = db;
        db.edges.put(this);
    }

    pragma(inline, true)
    override final inout(Db) db() inout { return _db; }
    private Db _db;             // up-reference
}

class Rela(uint arity) : Edge
    if (arity >= 2)
{
    @safe pure nothrow:

    this(Db db)
    {
        super(db);
    }

    Zing[arity] actors;
}

class Func(uint arity) : Edge
    if (arity >= 1)
{
    @safe pure nothrow:

    this(Db db)
    {
        super(db);
    }

    Zing[arity] params;
}

@safe pure nothrow unittest
{
    Db db = new Db();
    Node node = new Node(db);
    Edge edge = new Edge(db);
    auto rela2 = new Rela!2(db);
    auto func1 = new Func!1(db);
}
