/// Test https://forum.dlang.org/post/fwtgemakyefkkptxmlvl@forum.dlang.org

class Node {}

struct Tree
{
    Node root;
}

@safe unittest
{
    Node f() { auto  t = Tree(); return t.root; } // shouldn't this error aswell?
    Node g() { scope t = Tree(); return t.root; } // errors
}
