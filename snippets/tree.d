/// Test https://forum.dlang.org/post/fwtgemakyefkkptxmlvl@forum.dlang.org

@safe:

struct Tree(Node)
if (is(Node == class))
{
@safe:
    Node root() return scope pure nothrow @nogc { return _root; }
    void makeRoot() @trusted
    {
        import core.lifetime : emplace;
        emplace!Node(_store);
    }
private:
    Node _root;
    enum nodeSize = __traits(classInstanceSize, Node);
    void[nodeSize] _store;
}

class C { this() {} int x; }

@safe pure unittest
{
    C f() {       Tree!C t; return t.root; } // shouldn't this error aswell?
    C g() { scope Tree!C t; return t.root; } // errors
}
