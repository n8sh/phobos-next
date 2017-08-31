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
struct GrowOnlyUpwardsNaryTree(E)
{
    alias N = Node!E;

    /* @safe pure: */
public:

    /// Create with region size in bytes.
    this(size_t regionSize)
    {
        _allocator = Region!PureMallocator(regionSize);
    }

    this(in E e, size_t regionSize)
    {
        _allocator = Region!PureMallocator(regionSize);
        _root = makeNode(e);
    }

    /// Returns: a new node.
    N* makeNode(in E e)
    {
        typeof(return) node = cast(typeof(_root))_allocator.allocate(N.sizeof);
        emplace!(N)(node, e);
        return node;
    }

    /// Returns: top node.
    inout(Node!E)* root() inout
    {
        return _root;
    }

private:
    N* _root;

    import std.conv : emplace;

    // import std.experimental.allocator.mallocator : Mallocator;
    import pure_mallocator : PureMallocator;
    import std.experimental.allocator.building_blocks.region : Region;

    Region!PureMallocator _allocator;
}

unittest
{
    struct X { string src; }

    auto tree = GrowOnlyUpwardsNaryTree!X(X("alpha"), 1024 * 1024);
}
