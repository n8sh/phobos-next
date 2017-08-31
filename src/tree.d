module tree;

/// Tree node.
struct Node(E)
{
private:
    Node *_root;
}

/// N-ary tree that can only grow (in breadth and depth).
struct GrowOnlyNaryTree(E)
{
public:
private:
    Node!E *_root;

    // See also: http://forum.dlang.org/post/prsxfcmkngfwomygmthi@forum.dlang.org
    import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator.building_blocks.region : Region;
}

@safe pure nothrow @nogc unittest
{
    struct X { string src; }
    GrowOnlyNaryTree!X tree;
}
