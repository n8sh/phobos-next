/// Test diagnostics for abstract members.
module abstract_members;

enum Rel { subkindOf, partOf }

@safe pure:

class Edge
{
    @safe pure:
    abstract Rel rel() const nothrow @nogc;
}

class SubkindOf : Edge
{
    @safe pure:
    override Rel rel() const nothrow @nogc
    {
        return Rel.subkindOf;
    }
}

class PartOf : Edge
{
    override Rel rel() const nothrow @nogc
    {
        return Rel.subkindOf;
    }
}

@safe pure nothrow unittest
{
    auto edge = new SubkindOf();
    assert(edge.rel == Rel.subkindOf);
}
