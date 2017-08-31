module tree;

/// Tree node.
struct Node(E)
{
private:
    E data;
    import array_ex : UniqueArray;
    UniqueArray!(Node!E*) subs;
}

/// N-ary tree that can only grow (in breadth and depth).
struct GrowOnlyNaryTree(E)
{
    /* @safe pure: */
public:
    this(size_t regionSize)
    {
        auto _allocator = Region!PureMallocator(regionSize);
    }
private:
    Node!E *_root;

    // See also: http://forum.dlang.org/post/prsxfcmkngfwomygmthi@forum.dlang.org
    // import std.experimental.allocator.mallocator : Mallocator;
    import pure_mallocator : PureMallocator;
    import std.experimental.allocator.building_blocks.region : Region;

    Region!PureMallocator _allocator;
}

unittest
{
    struct X { string src; }
    GrowOnlyNaryTree!X tree;
}
