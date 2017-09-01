module tree;

/** N-ary tree that cannot shrink but only grow (in breadth and depth).

    Because of this a region allocator can be used for internal memory
    allocation.

    See also: http://forum.dlang.org/post/prsxfcmkngfwomygmthi@forum.dlang.org
 */
struct GrowOnlyNaryTree(E)
{
    alias N = Node!E;

    /* @safe pure: */
public:

    /// Create with region size in bytes.
    this(size_t regionSize) @trusted
    {
        _allocator = Allocator(regionSize);
    }

    this(in E e, size_t regionSize) @trusted
    {
        _allocator = Allocator(regionSize);
        _root = _allocator.make!N(e);
    }

    /// Returns: top node.
    inout(Node!E)* root() inout return scope
    {
        return _root;
    }

private:
    N* _root;

    // import std.experimental.allocator.mallocator : Mallocator;
    import std.experimental.allocator : make, makeArray;
    import std.experimental.allocator.building_blocks.region : Region;
    import pure_mallocator : PureMallocator;
    alias Allocator = Region!PureMallocator;

    Allocator _allocator;
}

/*@safe*/ unittest
{
    struct X { string src; }

    const e = X("alpha");
    auto tree = GrowOnlyNaryTree!X(e, 1024 * 1024);
    assert(tree.root.data == e);

    // TODO this should fail to compile with -dip1000
    Node!X* dumb()
    {
        auto tree = GrowOnlyNaryTree!X(e, 1024 * 1024);
        return tree.root;
    }

    dumb();
}

/// Tree node containing `E`.
private struct Node(E)
{
private:
    E data;

    // allocate with pointers with std.experimental.allocator.makeArray and each
    // pointer with std.experimental.allocator.makeArray
    Node!E*[] subs;
}
