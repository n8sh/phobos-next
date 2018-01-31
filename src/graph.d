@safe pure nothrow:

import std.array : Appender;

import languages;

class Db
{
    Appender!(Node[]) nodes;
    Appender!(Edge[]) edges;
    Appender!(SuperEdge[]) superEdges;
}

extern(C++) class Entity
{
extern(D):
    @safe pure nothrow:
    abstract inout(Db) db() inout;           // get up-reference
}

extern(C++) class Node : Entity
{
extern(D):
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

extern(C++) class Text : Node
{
extern(D):
    @safe pure nothrow:
    this(Db db, string text, Lang lang)
    {
        super(db);
        this.text = text;
    }
    const string text;
    Lang lang;
}

/// Number with numerical type `T`.
extern(C++) class Number(T) : Node
{
extern(D):
    @safe pure nothrow:
    this(Db db, T value)
    {
        super(db);
        this.value = value;
    }
    const T value;
}

extern(C++) class Edge : Entity
{
extern(D):
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

extern(C++) class SuperEdge : Entity
{
extern(D):
    @safe pure nothrow:

    this(Db db)
    {
        _db = db;
        db.superEdges.put(this);
    }

    pragma(inline, true)
    override final inout(Db) db() inout { return _db; }
    private Db _db;             // up-reference
}

extern(C++) class Rela(uint arity) : Edge
    if (arity >= 2)
{
extern(D):
    @safe pure nothrow:

    this(Db db)
    {
        super(db);
    }

    Entity[arity] actors;
}

extern(C++) class Func(uint arity) : Edge
    if (arity >= 1)
{
extern(D):
    @safe pure nothrow:

    this(Db db)
    {
        super(db);
    }

    Entity[arity] params;
}

@safe pure nothrow unittest
{
    Db db = new Db();
    Node node = new Node(db);
    Edge edge = new Edge(db);
    auto rela2 = new Rela!2(db);
    auto func1 = new Func!1(db);
}
