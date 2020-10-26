/// Test https://forum.dlang.org/post/fwtgemakyefkkptxmlvl@forum.dlang.org

@safe:

struct Tree(Node)
if (is(Node == class))
{
@safe pure nothrow @nogc:
    Node root() return scope
    {
        return _root;
    }
private:
    Node _root;
    void* _store;
}

/++ C
 +/
class C
{
	this()
	{
	}
    int x;
}

@safe pure unittest
{
    C f()
    {
        Tree!C t;
        return t.root;
    }
    C g()
    {
        scope Tree!C t;
        return t.root;
    }
}
