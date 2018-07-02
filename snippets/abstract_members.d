/// Test diagnostics for abstract members.
module abstract_members;

enum Rel { subkindOf, partOf }

class Edge
{
    @safe pure:
    abstract Rel rel() const nothrow @nogc;
}

class SubkindOf : Edge
{
    @safe pure:
}

@safe pure nothrow unittest
{
    auto subkindOf = new SubkindOf();
}
