module tree;

/// Tree node containing `E`.
private struct Node(E)
{
private:
    E data;
    import array_ex : UniqueArray;
    UniqueArray!(Node!E*) subs;
}

/** N-ary tree that cannot shrink but only grow (in breadth and depth).

    Because of this a region allocator can be used for internal memory
    allocation.

    See also: http://forum.dlang.org/post/prsxfcmkngfwomygmthi@forum.dlang.org
 */
struct GrowOnlyNaryTree(E)
{
    /* @safe pure: */
public:
    @disable this(this);

    this(size_t regionSize)
    {
        auto _allocator = Region!PureMallocator(regionSize);
    }
private:
    Node!E *_root;

    // import std.experimental.allocator.mallocator : Mallocator;
    import pure_mallocator : PureMallocator;
    import std.experimental.allocator.building_blocks.region : Region;

    Region!PureMallocator _allocator;
}

unittest
{
    struct X { string src; }

    auto tree = GrowOnlyNaryTree!X(1024);
}
