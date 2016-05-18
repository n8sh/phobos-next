/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree

    TODO Can we store Br2, Br4 and Br16 together in a variable length array?

    TODO How do we store difference between null and empty arrays?

    TODO Use std.experimental.allocator

    TODO Move construction params into construct!NodeType(params...)

    TODO
    - PLf: Word:
    - 64-bit:
    radixPow2-4: Packs 8-bit length + Variable (0 .. 14) IxMs
    radixPow2-8: Packs 8-bit length + Variable (0 .. 6) IxMs
    - 32-bit:
    radixPow2-4: Packs 8-bit length + Variable (0 .. 14) IxMs
    radixPow2-8: Packs 8-bit length + Variable (0 .. 2) IxMs
    - BrM: IxMs commonPrefix. IxMs is 7-byte BitSet.
    - SBr02: IxMs commonPrefix. IxMs is 7-byte BitSet.

    TODO - `set.prefix("alpha")`                           => `SortedTreeRange` of `Tuple!(string, Lang, PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha"))`                    => `SortedTreeRange` of `Tuple!(Lang, PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha", Lang.en))`           => `SortedTreeRange` of `Tuple!(PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha", Lang.en, PoT.noun))` => `SortedTreeRange` of `Tuple!(Sense)`.

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type BrM*?

    TODO Add sparse `2^^n`-branches for `n < radixPow2`: 2^^1=B2, 2^^2=B4, 2^^3=B8, B^^4=B16. Use
    sortExactly from sortn.d to order their members.

    TODO Make Br4 templated on N when I figure out how
    to elide the recursive template-instantiation. Ask forums

    TODO Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`
    TODO Provide RandomAccess `opIndex` and `opSlice`! for variable-length keys aswell?
    TODO Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to _root. Add checks with `isSorted`.

    TODO Should opBinaryRight return void* instead of bool for set-case?
*/
module trie;

import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy, isPointer;
import std.typecons : tuple, Tuple, Unqual;
import std.range : isInputRange, ElementType;

import bijections : isIntegralBijectableType, bijectToUnsigned;
import variant_ex : WordVariant;
import typecons_ex : IndexedArray, StrictlyIndexed;
import modulo : Mod, mod;

version = benchmark;

import dbg;

alias isFixedTrieableKeyType = isIntegralBijectableType;

enum isTrieableKeyType(T) = (isFixedTrieableKeyType!T ||
                             (isInputRange!T &&
                              isFixedTrieableKeyType!(ElementType!T)));

extern(C) pure nothrow @system /* TODO @nogc */
{
    void* malloc(size_t size);
    void* calloc(size_t nmemb, size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}

/** Index to chunk of bits. */
alias BIx = uint;

/** Raw Internal (Unsigned Integer) Binary Key. */
alias Key(size_t radixPow2) = Mod!(2^^radixPow2)[]; // TODO use bitset to more naturally support radixPow2 != 8
alias KeyN(size_t radixPow2, size_t N) = Mod!(2^^radixPow2)[N];

/** Size of a CPU cache line in bytes.

    Container layouts should be adapted to make use of at least this many bytes
    in its nodes.
 */
enum cacheLineSize = 64;

shared static this()
{
    import core.cpuid;
    assert(cacheLineSize == dataCaches()[0].lineSize, "Cache line is not 64 bytes");
}

/** Statically allocated `Ix`-array of maximum length `n`.
 */
struct IxsN(size_t maxLength,
            uint radixPow2 = 8)
    if (maxLength >= 2)         // no use storing less than 2 bytes
{
    enum M = 2^^radixPow2;     // branch-multiplicity, typically either 2, 4, 16 or 256
    alias Ix = Mod!M;

    this(Ix[] ixs)
    {
        assert(ixs.length <= maxLength);

        this.ixs[0 .. ixs.length] = ixs;
        this.length = cast(ubyte)ixs.length;
    }

    @property auto toString() const
    {
        import std.conv : to;
        return ixs[0 .. length].to!string;
    }

    @safe pure nothrow @nogc:

    bool empty() const { return length == 0; }
    bool full() const { return length == maxLength; }

    auto ref front() inout
    {
        assert(!empty);
        return ixs[0];
    }

    void popFront()
    {
        assert(!empty);

        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. length - 1)
        {
            ixs[i] = ixs[i + 1]; // TODO move construct?
        }
        --length;
    }

    void popBack() { assert(!empty); --length; }
    void pushBack(Ixs...)(Ixs moreIxs)
        if (Ixs.length <= maxLength)
    {
        assert(!full);
        foreach (const i, const ix; moreIxs)
        {
            this.ixs[length + i] = ix;
        }
        length += Ixs.length;
    }

    auto chunks() inout { return ixs[0 .. length]; }
    alias chunks this;
private:
    ubyte length;               // number of defined elements in ixs
    Ix[maxLength] ixs;          // indexes
}

static assert(IxsN!(6, 8).sizeof == 7);

@safe pure nothrow unittest
{
    import std.algorithm : equal;
    import modulo : mod;

    enum radixPow2 = 8;
    enum M = 2^^radixPow2;

    alias Ix = Mod!(M, ubyte);
    Ix[] ixs = [11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M];
    enum maxLength = 7;
    auto plf = IxsN!(maxLength, radixPow2)(ixs);

    assert(plf.length == 4);
    assert(!plf.empty);

    assert(plf.equal([11, 22, 33, 44]));
    assert(!plf.full);
    plf.popFront;
    assert(plf.equal([22, 33, 44]));
    assert(!plf.full);
    plf.popBack;
    assert(plf.equal([22, 33]));
    assert(!plf.full);
    plf.popFront;
    assert(plf.equal([33]));
    assert(!plf.full);
    plf.popFront;
    assert(plf.empty);
    assert(!plf.full);
    assert(plf.length == 0);

    plf.pushBack(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M, 55.mod!M, 66.mod!M, 77.mod!M);
    assert(plf.equal([11, 22, 33, 44, 55, 66, 77]));
    assert(!plf.empty);
    assert(plf.full);
}

/** Raw radix tree container storing untyped variable-length `Key`.

    In set-case (`Value` is `void`) this container is especially suitable for
    representing a set of 32 or 64 integers/pointers.

    Radix-trees are suitable for storing variable-keys and provide completion of
    all keys matching a given key prefix. This enables efficient storage of long
    URLs sharing a common prefix, typically a domain and path.

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree
*/
struct RawRadixTree(Value,
                    uint radixPow2 = 8) // binary power of radix, typically either 1, 2, 4 or 8
{
    import std.bitmanip : bitfields;
    import std.conv : to;
    import std.algorithm : filter;
    import std.meta : AliasSeq, staticMap;
    import std.typecons : ConstOf;

    static assert(radixPow2 == 8, "Radix is currently limited to 8");

    enum isSet = is(Value == void); // `true` if this tree is a set. TODO better to use empty struct?
    enum isMap = !isSet;            // `true` if this tree is a map

    enum M = 2^^radixPow2;     // branch-multiplicity, typically either 2, 4, 16 or 256

    alias order = M;   // tree order

    /// `true` if tree has binary branch.
    enum isBinary = radixPow2 == 2;

    /** Radix Modulo Index */
    alias Ix = Mod!M; // restricted index type avoids range checking in array indexing below

    /** `radixPow2` least significant bits (LSB) of leaves directly packed into a word.

        TODO Generalize to packing of more than one `Ix` per byte.
        TODO respect byteorder in `PLfs` to work with `WordVariant`
        TODO implement and use opSlice instead of .suffix[]
    */
    static if (size_t.sizeof == 8) // 64-bit CPU
    {
        static if (radixPow2 == 8)
        {
            // Packed Variable-Length Single Leaf
            struct PLf
            {
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof;
                this(Ix[] ixs) { this.chunks = ixs; }
                IxsN!(maxLength, radixPow2) chunks;
                alias chunks this;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
                // static if (isMap)
                // {
                //     static if (is(Value == bool))
                //         static assert(false, "TODO store bit packed");
                //     else
                //         Value[maxLength] values;
                // }
            }

            struct PLfs
            {
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof;
                Ix[maxLength] ixMs;
                ubyte length;
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
                // static if (isMap)
                // {
                //     static if (is(Value == bool))
                //     {
                //         import bitset : BitSet;
                //         BitSet!maxLength values; // memory-efficient storage of `bool` values
                //     }
                //     else
                //         Value[maxLength] values;
                // }
            }
        }

        // TODO handle radixPow2 != 8
        static if (isMap && is(Value == bool))
        {
            /* TODO pack bit efficiently */
        }
    }
    else
    {
        static assert(false, "Currently requires a 64-bit CPU (size_t.sizeof == 8)");
    }

    // TODO make these CT-params (requires putting branch definitions in same scope as `RawRadixTree`)
    static if (radixPow2 == 8)
    {
        alias DefaultRootType = BrM*;
    }
    else static if (radixPow2 == 4)
    {
        alias DefaultRootType = Br4*;
    }

    alias DefaultBr = DefaultRootType;
    alias DefaultLf = PLfs; // TODO use either LfM* or PLfs instead

    static if (isSet)
        static assert(PLfs.sizeof == size_t.sizeof); // assert that it's size matches platform word-size

    /** Node types. */
    alias NodeTypes = AliasSeq!(PLf, // dense leaf
                                PLfs, // sparse leaves

                                // sparse branches
                                Br2*,
                                Br4*,

                                BrM*,  // dense branches

                                LfM*); // dense leaves

    /** Mutable node. */
    alias Node = WordVariant!NodeTypes;
    /** Constant node. */
    // TODO make work with indexNaming alias ConstNodePtr = WordVariant!(staticMap!(ConstOf, NodeTypes));

    static assert(radixPow2 <= 8*Ix.sizeof, "Need more precision in Ix");

    /** Tree Leaf Iterator. */
    struct It
    {
        bool opCast(T : bool)() const @safe pure nothrow /* TODO @nogc */ { return cast(bool)node; }
        Node node;              // current leaf-`Node`. TODO use `Lf` type instead?
        Ix ixM;                // index to sub at `node`
    }

    /** Tree Key Find Result. */
    struct KeyFindResult // TODO shorter naming
    {
        /* this save 8 bytes and makes this struct 16 bytes instead of 24 bytes
           compared using a member It instead of Node and Ix */
        auto it() { return It(node, ixM); }
        bool opCast(T : bool)() const @safe pure nothrow /* TODO @nogc */ { return hit; }
        Node node;
        Ix ixM;
        bool hit;
    }

    // TODO move these definitions inside branch definitions?

    /** M-Branch population histogram.
        Index maps to population with value range (1 .. `M`).
    */
    alias BrM_PopHist = size_t[M];

    /** 2-Branch population histogram.
        Index maps to population with value range (1 .. 2).
    */
    alias Br2_PopHist = size_t[2];

    /** 4-Branch population histogram.
        Index maps to population with value range (1 .. 4).
    */
    alias Br4_PopHist = size_t[4];

    /** M-Leaf population histogram.
        Index maps to population with value range (1 .. `M`).
    */
    alias LeafM_PopHist = size_t[M];

    /** Tree Population and Memory-Usage Statistics. */
    struct Stats
    {
        Br2_PopHist popHist_Br2;
        Br4_PopHist popHist_Br4;
        BrM_PopHist popHist_BrM;
        LeafM_PopHist popHist_LfM;

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));
    }

    enum brMPrefixLength = 15; // we can afford larger prefix here because BrM is so large

    /** Dense/Unpacked `M`-Branch with `M` number of sub-nodes. */
    static private struct BrM
    {
        IxsN!brMPrefixLength prefix; // prefix (edge-label) common to all `subNodes`
        bool isKey;      // key at this branch is occupied
        StrictlyIndexed!(Node[M]) subNodes;

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow /* TODO @nogc */ const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Value, radixPow2)(stats);
            }
            ++stats.popHist_BrM[nnzSubCount - 1]; // TODO type-safe indexing
        }

        // LfM subOccupations; // if i:th bit is set key (and optionally value) associated with sub[i] is also defined
        // static if (isMap) { Value value; }
    }

    /** Sparse/Packed full 2-way branch.

        A 2-branch always always contain two defined sub-nodes (`full` always
        returns `true`). If more are added it is automatically expanded to
        larger branch, typicall 4-branch `Br4`.
     */
    static private struct Br2
    {
        enum N = 2; // TODO make this a CT-param when this structu is moved into global scope

        // members in order of decreasing alignof:
        StrictlyIndexed!(Node[N]) subNodes;
        IxsN!7 prefix; // prefix (edge-label) common to all `subNodes`
        StrictlyIndexed!(Ix[N]) subIxs;
        bool isKey;             // key at this branch is occupied

        @safe pure nothrow:
        inout(Node) findSub(Ix ix) inout
        {
            if (subIxs.at!0 == ix) { return subNodes.at!0; }
            if (subIxs.at!1 == ix) { return subNodes.at!1; }
            return Node.init;
        }
        enum empty = false;
        enum full = true;
        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) const
        {
            const nnzSubCount = 2; // number of non-zero sub-nodes
            subNodes.at!0.calculate!(Value, radixPow2)(stats);
            subNodes.at!1.calculate!(Value, radixPow2)(stats);
            ++stats.popHist_Br2[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    /** Sparse/Packed 4-way branch. */
    static private struct Br4
    {
        enum N = 4; // TODO make this a CT-param when this structu is moved into global scope

        // members in order of decreasing alignof:
        StrictlyIndexed!(Node[N]) subNodes;
        IxsN!7 prefix; // prefix common to all `subNodes` (also called edge-label)
        StrictlyIndexed!(Ix[N]) subIxs;
        mixin(bitfields!(ubyte, "subCount", 7, // counts length of defined elements in subNodes
                         bool, "isKey", 1)); // key at this branch is occupied

        @safe pure nothrow:
        void pushBackSub(Tuple!(Ix, Node) sub)
        {
            assert(!full);
            const backIx = subCount.mod!N;
            subIxs[backIx] = sub[0];
            subNodes[backIx] = sub[1];
            subCount = cast(ubyte)(subCount + 1);
        }
        inout(Node) findSub(Ix ix) inout
        {
            foreach (const i_; 0 ..  subCount)
            {
                const i = i_.mod!N;
                if (subIxs[i] == ix) { return subNodes[i]; }
            }
            return Node.init;
        }
        const:
        bool empty() @nogc { return subCount == 0; }
        bool full() @nogc { return subCount == N; }
        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats)
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[0 .. subCount])
            {
                ++nnzSubCount;
                sub.calculate!(Value, radixPow2)(stats);
            }
            ++stats.popHist_Br4[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    static if (false)
    {
        pragma(msg, "Br2.sizeof:", Br2.sizeof, " Br2.alignof:", Br2.alignof);
        pragma(msg, "Br4.sizeof:", Br4.sizeof, " Br4.alignof:", Br4.alignof);

        pragma(msg, "Br2.subNodes.sizeof:", Br2.subNodes.sizeof, " Br2.subNodes.alignof:", Br2.subNodes.alignof);
        pragma(msg, "Br4.subNodes.sizeof:", Br4.subNodes.sizeof, " Br4.subNodes.alignof:", Br4.subNodes.alignof);

        pragma(msg, "Br2.prefix.sizeof:", Br2.prefix.sizeof, " Br2.prefix.alignof:", Br2.prefix.alignof);
        pragma(msg, "Br4.prefix.sizeof:", Br4.prefix.sizeof, " Br4.prefix.alignof:", Br4.prefix.alignof);

        pragma(msg, "Br2.subIxs.sizeof:", Br2.subIxs.sizeof, " Br2.subIxs.alignof:", Br2.subIxs.alignof);
        pragma(msg, "Br4.subIxs.sizeof:", Br4.subIxs.sizeof, " Br4.subIxs.alignof:", Br4.subIxs.alignof);
    }

    /** Set sub-`Node` of branch `Node br` at index `ix` to `subNode`. */
    Node setSub(Node br, Ix subIx, Node subNode)
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_Br2Ptr:
            auto br2 = br.as!(Br2*);
            if (auto existingSubNode = br2.findSub(subIx))
            {
                assert(existingSubNode == subNode, "Existing subNode differs from parameter");
                return br;      // already added
            }
            else
            {
                return setSub(cast(Node)expand(br2), subIx, subNode); // always expand
            }
            break;
        case Node.Ix.ix_Br4Ptr:
            auto br4 = br.as!(Br4*);
            if (auto existingSubNode = br4.findSub(subIx))
            {
                assert(existingSubNode == subNode, "Existing subNode differs from parameter");
                return br;      // already added
            }
            if (br4.full) { return setSub(cast(Node)expand(br4), subIx, subNode); } // expand if needed
            br4.pushBackSub(tuple(subIx, subNode));
            break;
        case Node.Ix.ix_BrMPtr:
            br.as!(BrM*).subNodes[subIx] = subNode;
            break;
        default: assert(false, "Unsupported Node type");
        }
        return br;
    }

    /** Get sub-`Node` of branch `Node br` at index `ix. */
    inout(Node) getSub(inout Node br, Ix ix)
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_Br2Ptr: if (auto subNode = br.as!(Br2*).findSub(ix)) { return subNode; } break;
        case Node.Ix.ix_Br4Ptr: if (auto subNode = br.as!(Br4*).findSub(ix)) { return subNode; } break;
        case Node.Ix.ix_BrMPtr: return br.as!(BrM*).subNodes[ix];
        default: assert(false, "Unsupported Node type");
        }
        return Node.init;
    }

    /** Returns: `true` if `br` is occupied, `false` otherwise. */
    bool isKey(Node br) const
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_Br2Ptr: return br.as!(Br2*).isKey;
        case Node.Ix.ix_Br4Ptr: return br.as!(Br4*).isKey;
        case Node.Ix.ix_BrMPtr: return br.as!(BrM*).isKey;
        default: assert(false, "Unsupported Node type");
        }
    }

    /** Get prefix of branch node `br`. */
    auto prefix(inout Node br)
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_Br2Ptr: return br.as!(Br2*).prefix[];
        case Node.Ix.ix_Br4Ptr: return br.as!(Br4*).prefix[];
        case Node.Ix.ix_BrMPtr: return br.as!(BrM*).prefix[];
        default: assert(false, "Unsupported Node type");
        }
    }

    /** Bottom-most leaf node of tree storing `M` number of densly packed keys
        of fixed-length type `Key`.
    */
    static private struct LfM
    {
        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure const
        {
            ++stats.popHist_LfM[keyLSBits.countOnes - 1];
        }

        import bitset : BitSet;
        private BitSet!M keyLSBits; // if i:th bit is set, then corresponding sub is set

        static if (isMap)
        {
            static if (is(Value == bool))
                BitSet!M values; // memory-efficient storage of `bool` values
            else
                Value[M] values;
        }
    }

    Stats usageHistograms() const
    {
        typeof(return) stats;
        _root.calculate!(Value, radixPow2)(stats);
        return stats;
    }

    this(this)
    {
        if (!_root) return;
        auto rhsRoot = _root;
        debug const oldLength = _length;
        if (rhsRoot)
        {
            _root = null;       // reset
            _length = 0;        // needs reset because insert updates
            // TODO insert(rhsRoot[]);
        }
        assert(false, "TODO calculate tree by branches and leafs and make copies of them");
    }

    ~this()
    {
        if (_root) { release(_root); }
        assert(_nodeCount == 0);
    }

    @safe pure nothrow /* TODO @nogc */
    {
        /** Insert `key` into `this` tree. */
        pragma(inline) Node insert(Key!radixPow2 key, out bool wasAdded)
        {
            return _root = insertAt(_root, key, wasAdded);
        }

        pragma(inline) Node insertNew(Key!radixPow2 key, out bool wasAdded)
        {
            if (key.length <= PLf.maxLength)
            {
                PLf currPLf = construct!(PLf)(key);
                wasAdded = true;
                return Node(currPLf);
            }
            else // key doesn't fit in a `PLf`
            {
                // TODO functionize
                BrM* br = construct!(DefaultBr)(key[0 .. key.length - PLf.maxLength].to!(typeof(DefaultBr.prefix)));
                key = key[br.prefix.length .. $];
                assert(false, "TODO test");
                return Node(br); // use this branch below in this function to insert into
            }
        }

        /** Insert `key` into sub-tree under root `curr`. */
        pragma(inline) Node insertAt(Node curr, Key!radixPow2 key, out bool wasAdded) // Node-polymorphic
        {
            if (!curr)          // if no existing `Node` to insert at
            {
                curr = insertNew(key, wasAdded);
                if (wasAdded) { return curr; } // we're done so return directly
            }

            with (Node.Ix)
            {
                final switch (curr.typeIx)
                {
                case undefined: break;
                case ix_PLf:    return insertAt(curr.as!(PLf), key, wasAdded);
                case ix_PLfs:   return insertAt(curr.as!(PLfs), key, wasAdded);
                case ix_Br2Ptr: return insertAt(curr.as!(Br2*), key, wasAdded);
                case ix_Br4Ptr: return insertAt(curr.as!(Br4*), key, wasAdded);
                case ix_BrMPtr: return insertAt(curr.as!(BrM*), key, wasAdded);
                case ix_LfMPtr: return insertAt(curr.as!(LfM*), key, wasAdded);
                }
                assert(false);
            }
        }

        pragma(inline) Node insertAtBranch(Node curr, Key!radixPow2 key, out bool wasAdded) // Node-polymorphic
        {
            // TODO move logic from insertAt(BrM and use members setSub, getSub, isOccupied getPrefix to access)
            return curr;
        }

        /** Insert `key` into sub-tree under root `curr`. */
        Node insertAt(Br2* curr, Key!radixPow2 key, out bool wasAdded)
        {
            assert(false, "TODO sync with changes in insertAt(BrM*");
        }

        /** Insert `key` into sub-tree under root `curr`. */
        Node insertAt(Br4* curr, Key!radixPow2 key, out bool wasAdded)
        {
            assert(false, "TODO sync with changes in insertAt(BrM*");
            // const Ix ix = key[0];
            // enum N = Br4.N;         // branch-order, number of possible sub-nodes
            // foreach (Mod!N ix; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            // {
            //     if (curr.subNodes[ix])   // first is occupied
            //     {
            //         if (curr.subIxs[ix] == ix) // and matches ix
            //         {
            //             curr.subNodes[ix] = insertAt(curr.subNodes[ix], key[1 .. $], wasAdded);
            //             return Node(curr);
            //         }
            //     }
            //     else            // use first free sub
            //     {
            //         auto subkey = key[1 .. $];
            //         curr.subNodes[ix] = insertAt(constructSub(subkey),
            //                                         subkey, wasAdded); // use it
            //         curr.subIxs[ix] = ix;
            //         return Node(curr);
            //     }
            // }
            // // if we got here all N sub-nodes are occupied so we need to expand
            // return insertAt(expand(curr), key, wasAdded); // NOTE stay on same depth
        }

        Node insertAt(BrM* curr, Key!radixPow2 key, out bool wasAdded)
        {
            import std.algorithm : commonPrefix;
            auto matchedPrefix = commonPrefix(key, curr.prefix);

            // prefix:abcd, key:ab
            if (matchedPrefix.length == key.length &&
                matchedPrefix.length < curr.prefix.length) // prefix is an extension of key
            {
                DefaultBr br = construct!(DefaultBr)(matchedPrefix.to!(typeof(DefaultBr.prefix)),
                                                     true); // `true` because `key` occupies this node
                br.subNodes[curr.prefix[matchedPrefix.length]] = curr;
                curr.prefix = curr.prefix[matchedPrefix.length + 1 .. $].to!(typeof(BrM.prefix)); // drop matchedPrefix plus index
                return Node(br);
            }
            // prefix:ab, key:abcd
            else if (matchedPrefix.length == curr.prefix.length &&
                     matchedPrefix.length < key.length) // key is an extension of prefix
            {
                key = key[matchedPrefix.length .. $]; // strip `curr.prefix from beginning of `key`
                // continue below
            }
            // prefix:ab, key:ab
            else if (matchedPrefix.length == curr.prefix.length && // exact key prefix match
                     matchedPrefix.length == key.length)
            {
                if (!curr.isKey)
                {
                    curr.isKey = true;
                    wasAdded = true;
                }
                return Node(curr);
            }
            // prefix:ab, key:cd
            else if (matchedPrefix.length == 0) // no prefix key match
            {
                if (curr.prefix.length == 0) // no current prefix
                {
                    // continue below
                }
                else
                {
                    BrM* br = construct!(DefaultBr);
                    br.subNodes[curr.prefix[0]] = curr;
                    curr.prefix = curr.prefix[1 .. $].to!(typeof(DefaultBr.prefix));
                    auto node = insertAt(br, key, wasAdded);
                    return node;
                }
            }

            const ix = key[0];
            curr.subNodes[ix] = insertAt(curr.subNodes[ix], key[1 .. $], wasAdded); // recurse
            return Node(curr);
        }

        Node insertAt(LfM* curr, Key!radixPow2 key, out bool wasAdded)
        {
            const ix = key[0];
            if (!curr.keyLSBits[ix])
            {
                curr.keyLSBits[ix] = true;
                wasAdded = true;
            }
            else
            {
                wasAdded = false;
            }
            return Node(curr);
        }

        Node insertAt(PLf curr, Key!radixPow2 key, out bool wasAdded)
        {
            import std.range : empty;
            import std.algorithm : commonPrefix;

            if (key.empty) { return Node(curr); }

            auto prefix = commonPrefix(key, curr.chunks);
            if (prefix.length == key.length &&
                prefix.length == curr.chunks.length) // key already stored
            {
                return Node(curr); // already stored in `curr`
            }
            else
            {
                return insertAt(splice(curr, prefix), key, wasAdded);
            }
        }

        /** Splice `curr` using `prefix`. */
        Node splice(PLf curr, Key!radixPow2 prefix)
        {
            auto br = construct!(DefaultBr)(prefix.to!(typeof(DefaultBr.prefix)));

            bool wasAdded;      // dummy
            auto node = insertAt(br, curr.chunks, wasAdded);
            assert(wasAdded); // assure that existing key was reinserted
            freeNode(curr);   // remove old current

            return node;
        }

        Node insertAt(PLfs curr, Key!radixPow2 key, out bool wasAdded)
        {
            const Ix ix = key[0];

            // TODO this is only marginally faster:
            // foreach (const i; iota!(0, curr.maxLength))
            // {
            //     if (i == curr.length) break;
            //     else if (curr.ixMs[i] == ix) { return Node(curr); }
            // }

            import std.algorithm.searching : canFind;
            if (curr.ixMs[0 .. curr.length].canFind(ix)) // if already stored. TODO use binarySearch
            {
                return Node(curr); // already there, so return current node as is
            }

            if (curr.length < curr.maxLength) // if there's room left in curr
            {
                curr.ixMs[curr.length++] = ix;
                // import sortn : networkSortUpTo;
                // TODO curr.ixMs[0 .. length].networkSort;
                // TODO curr.ixMs[0 .. length].sort;
                wasAdded = true;
                return Node(curr); // current node still ok
            }
            else
            {
                return insertAt(expand(curr), key, wasAdded); // NOTE stay at same (depth)
            }
        }

        /** Construct and return sub-Node at `key`.  */
        Node constructSub(Key!radixPow2 key)
        {
            import std.range : empty;
            if (key.empty)
            {
                return Node(construct!(PLf));
            }
            else
            {
                const bool isLast = key.length == 1;
                return (isLast ?
                        Node(construct!(DefaultLf)) :
                        Node(construct!(DefaultBr)));
            }
        }

        /** Destructively expand `curr` of type `Br2` into a `Br4` and return it. */
        Br4* expand(Br2* curr)
        {
            auto next = construct!(typeof(return));
            foreach (const i; iota!(0, curr.N)) // each sub node. TODO use iota!(Mod!N)
            {
                next.subNodes.at!i = curr.subNodes.at!i;
                next.subIxs.at!i = curr.subIxs.at!i;
            }
            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` of type `Br4` into a `BrM` and return it. */
        BrM* expand(Br4* curr)
        {
            auto next = construct!(typeof(return));
            foreach (Mod!(curr.N) i; iota!(0, curr.N)) // each sub node. TODO use iota!(Mod!N)
            {
                next.subNodes[curr.subIxs.at!i] = curr.subNodes.at!i;
            }
            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` into a `LfM` and return it. */
        LfM* expand(PLfs curr)
        {
            auto next = construct!(typeof(return));
            foreach (const ixM; curr.ixMs[0 .. curr.length])
            {
                assert(!next.keyLSBits[ixM]); // assert no duplicates in ixMs
                next.keyLSBits[ixM] = true;
            }
            freeNode(curr);
            return next;
        }

    }

    /** Returns: `true` iff tree is empty (no elements stored). */
    bool empty() const @safe pure nothrow /* TODO @nogc */ { return !_root; }

    /** Returns: number of elements store. */
    size_t length() const @safe pure nothrow /* TODO @nogc */ { return _length; }

    private:

    /** Allocate (if pointer) and Construct a `Node`-type of value type `U`
        using constructor arguments `args` of `Args`. */
    auto construct(U, Args...)(Args args) @trusted
    {
        debug ++_nodeCount;
        static if (isPointer!U)
        {
            import std.conv : emplace;
            auto node = emplace(cast(U)malloc((*U.init).sizeof), args);
            // TODO ensure alignment of node at least that of U.alignof
            return node;
        }
        else
        {
            return U(args);
        }
    }

    void freeNode(NodeType)(NodeType nt) @trusted
    {
        static if (isPointer!NodeType)
        {
            free(cast(void*)nt);  // TODO Allocator.free
        }
        debug --_nodeCount;
    }

    @safe pure nothrow /* TODO @nogc */
    {
        void release(BrM* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(Br2* curr)
        {
            release(curr.subNodes.at!0); // recurse
            release(curr.subNodes.at!1); // recurse
            freeNode(curr);
        }

        void release(Br4* curr)
        {
            foreach (sub; curr.subNodes[0 .. curr.subCount])
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(PLf curr) { freeNode(curr); }
        void release(PLfs curr) { freeNode(curr); }
        void release(LfM* curr) { freeNode(curr); }

        void release(Node curr)
        {
            with (Node.Ix)
            {
                final switch (curr.typeIx)
                {
                case undefined: break;
                case ix_PLf: return release(curr.as!(PLf));
                case ix_PLfs: return release(curr.as!(PLfs));
                case ix_Br2Ptr: return release(curr.as!(Br2*));
                case ix_Br4Ptr: return release(curr.as!(Br4*));
                case ix_BrMPtr: return release(curr.as!(BrM*));
                case ix_LfMPtr: return release(curr.as!(LfM*));
                }
            }
        }
    }

    /** Ensure that root `Node` is allocated. */
    void ensureRootNode(U = DefaultRootType)()
    {
        if (!_root) { _root = construct!(U); }
    }

    /// Returns: number of nodes used in `this` tree.
    pragma(inline) debug size_t nodeCount() @safe pure nothrow /* TODO @nogc */ { return _nodeCount; }

    private Node _root;
    size_t _length = 0;

    debug size_t _nodeCount = 0;
}

/** Append statistics of tree under `Node` `sub.` into `stats`.
 */
static private void calculate(Value, uint radixPow2)(RawRadixTree!(Value, radixPow2).Node sub,
                                                   ref RawRadixTree!(Value, radixPow2).Stats stats)
    @safe pure nothrow /* TODO @nogc */
{
    alias RT = RawRadixTree!(Value, radixPow2);
    ++stats.popByNodeType[sub.typeIx];

    with (RT.Node.Ix)
    {
        final switch (sub.typeIx)
        {
        case undefined: break;
        case ix_PLf: break; // TODO calculate()
        case ix_PLfs: break; // TODO calculate()
        case ix_Br2Ptr: sub.as!(RT.Br2*).calculate(stats); break;
        case ix_Br4Ptr: sub.as!(RT.Br4*).calculate(stats); break;
        case ix_BrMPtr: sub.as!(RT.BrM*).calculate(stats); break;
        case ix_LfMPtr: sub.as!(RT.LfM*).calculate(stats); break;
        }
    }
}

/// Radix-Tree with key-type `Key` and value-type `Value`.
struct RadixTree(Key, Value, uint radixPow2 = 8)
    if (allSatisfy!(isTrieableKeyType, Key))
{
    /** Insert `key`.
        Returns: `true` if `key` wasn't previously inserted, `false` otherwise.
     */
    bool insert(in Key typedKey)
        @safe pure nothrow /* TODO @nogc */
    {
        // convert unsigned to fixed-length (on the stack) ubyte array

        // TODO functionize
        static if (isFixedTrieableKeyType!Key)
        {
            const ukey = typedKey.bijectToUnsigned;
            enum nbits = 8*ukey.sizeof;
            enum chunkCount = nbits/radixPow2;

            KeyN!(radixPow2, Key.sizeof) key;

            static if (radixPow2 == 8)
            {
                foreach (bix; 0 .. chunkCount)
                {
                    const bitShift = (chunkCount - 1 - bix)*radixPow2; // most significant bit chunk first (MSBCF)
                    key[bix] = (ukey >> bitShift) & (M - 1); // part of value which is also an index
                }
            }
        }
        else static if (is(Unqual!Key == string))
        {
            const ubyte[] key = Key.representation;
        }
        else static if (is(Unqual!Key == wstring))
        {
            const ushort[] key = Key.representation;
            assert(false, "TODO convert key to ubyte[]");
        }
        else static if (is(Unqual!Key == dstring))
        {
            const uint[] key = Key.representation;
            assert(false, "TODO convert key to ubyte[]");
        }
        else
        {
            assert(false, "TODO");
        }

        bool wasAdded = false; // indicates that key was added
        _tree.insert(key[], wasAdded);

        _length += wasAdded;
        return wasAdded;
    }

    static if (_tree.isSet)
    {
        /** Returns: `true` if key is contained in set, `false` otherwise. */
        bool contains(in Key key) const nothrow
        {
            return false;
        }

	/** Supports $(B `Key` in `this`) syntax.
            TODO return `NodeRef` instead.
        */
	bool opBinaryRight(string op)(in Key key) const nothrow if (op == "in") { return contains(key); }
    }

    static if (_tree.isMap)
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        bool insert(in Key key, Value value)
        {
            bool result = insert(key);
            // TODO call insertAtSubNode(result, value);
            return result;
        }
        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        Value* contains(in Key key) const
        {
            return null;
        }
    }

    private RawRadixTree!(Value, radixPow2) _tree;
    alias _tree this;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator of set-version of `RadixTree` where value-type is `void` (unused).
auto radixTreeSet(Key, uint radixPow2 = 4)() { return RadixTree!(Key, void, radixPow2)(); }

/// Instantiator of map-version of `RadixTree` where value-type is `Value`.
auto radixTreeMap(Key, Value, uint radixPow2 = 4)() { return RadixTree!(Key, Value, radixPow2)(); }

@safe pure nothrow /* TODO @nogc */ unittest
{
    enum radixPow2 = 8;
    auto set = radixTreeSet!(ubyte, radixPow2);

    assert(set.insert(0));
    assert(!set.insert(0));
    assert(set.nodeCount == 1); // one leaf

    assert(set.insert(1));
    assert(!set.insert(1));
    assert(set.nodeCount == 3); // one branch two leaves
}

/// Check correctness when radixPow2 is `radixPow2` and for each `Key` in `Keys`.
auto check(uint radixPow2, Keys...)()
    if (Keys.length >= 1)
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        import std.algorithm : equal;
        struct TestValueType { int i; float f; string s; }
        alias Value = TestValueType;
        import std.meta : AliasSeq;
        foreach (Key; Keys)
        {
            dln("Key: ", Key.stringof);
            alias Tree = radixTreeSet!(Key, radixPow2);
            auto set = Tree;
            assert(set.empty);

            static assert(set.isSet);

            import std.algorithm : min, max;

            const low = max(Key.min, -100_000);
            const high = min(Key.max, 100_000);
            const length = high - low + 1;

            const useContains = false;
            size_t cnt = 0;
            foreach (const uk; low.iota(high + 1))
            {
                const Key key = cast(Key)uk;
                if (useContains)
                {
                    assert(!set.contains(key)); // key should not yet be in set
                    assert(key !in set);        // alternative syntax
                }

                const show = false;
                if (show) { dln("============================= NEW INSERT of key:", key); }
                assert(set.insert(key));  // insert new value returns `true` (previously not in set)

                static if (Key.sizeof <= Tree.PLf.maxLength)
                {
                    switch (cnt)             // if first
                    {
                    case 0:                                // after first insert
                        assert(set._root.peek!(Tree.PLf)); // top should be a leaf
                        break;
                    case 1:                                 // after second insert
                        assert(set._root.peek!(Tree.BrM*)); // top should be a branch
                        break;
                    default: break;
                    }
                }

                if (show) { dln("============================= EXISTING INSERT of key:", key); }
                assert(!set.insert(key)); // reinsert same value returns `false` (already in set)

                if (useContains)
                {
                    assert(set.contains(key)); // key should now be in set
                    assert(key in set);        // alternative syntax
                    if (key != Key.max)        // except last value
                    {
                        assert(!set.contains(cast(Key)(key + 1))); // next key is not yet in set
                    }
                }
                ++cnt;
            }

            assert(set.length == length);

            auto map = radixTreeMap!(Key, Value, radixPow2);
            static assert(map.isMap);

            map.insert(Key.init, Value.init);
        }
    }
}

/// Benchmark performance and memory usage when radixPow2 is `radixPow2`.
void benchmark(uint radixPow2)()
{
    import core.thread : sleep;
    import std.range : iota;

    import std.algorithm : equal;
    struct TestValueType { int i; float f; string s; }
    alias Value = TestValueType;
    import std.meta : AliasSeq;
    foreach (Key; AliasSeq!(uint)) // just benchmark uint for now
    {
        auto set = radixTreeSet!(Key, radixPow2);
        alias Set = set;
        assert(set.empty);

        static assert(set.isSet);

        import std.conv : to;
        import std.datetime : StopWatch, AutoStart, Duration;

        enum n = 10_000_000;

        import std.array : array;
        import std.random : randomShuffle;

        const useUniqueRandom = false;

        // TODO functionize to randomIota in range_ex.d
        auto randomIotaSamples = 0.iota(n).array; randomIotaSamples.randomShuffle;

        import std.range : generate, take;
        import std.random : uniform;
        auto randomSamples = generate!(() => uniform!Key).take(n);

        {
            auto sw = StopWatch(AutoStart.yes);

            foreach (Key k; randomSamples)
            {
                if (useUniqueRandom)
                    assert(set.insert(k));
                else
                    set.insert(k);
            }

            dln("trie: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            auto stats = set.usageHistograms;
            dln("Sparse 2-Branch Population Histogram: ", stats.popHist_Br2);
            dln("Sparse 4-Branch Population Histogram: ", stats.popHist_Br4);
            dln("Dense M=", 2^^radixPow2, "-Branch Population Histogram: ", stats.popHist_BrM);
            dln("Dense M=", 2^^radixPow2, "-Leaf   Population Histogram: ", stats.popHist_LfM);
            dln("Population By Node Type: ", stats.popByNodeType);

            size_t totalBytesUsed = 0;
            foreach (Set.Node.Ix ix, pop; stats.popByNodeType) // TODO use stats.byPair when added to typecons_ex.d
            {
                size_t bytesUsed = 0;
                with (Set.Node.Ix)
                {
                    final switch (ix)
                    {
                    case undefined: break;
                    case ix_PLf:   bytesUsed = pop*Set.PLf.sizeof; break;
                    case ix_PLfs:   bytesUsed = pop*Set.PLfs.sizeof; break;
                    case ix_Br2Ptr: bytesUsed = pop*Set.Br2.sizeof; break;
                    case ix_Br4Ptr: bytesUsed = pop*Set.Br4.sizeof; break;
                    case ix_BrMPtr: bytesUsed = pop*Set.BrM.sizeof; break;
                    case ix_LfMPtr: bytesUsed = pop*Set.LfM.sizeof; break;
                    }
                }
                totalBytesUsed += bytesUsed;
                if (bytesUsed)
                {
                    dln(pop, " number of ", ix, " uses ", bytesUsed/1e6, " megabytes");
                }
            }
            dln("Tree uses ", totalBytesUsed/1e6, " megabytes");

            sleep(2);
            dln("Sleeping done");
        }

        {
            auto sw = StopWatch(AutoStart.yes);
            bool[int] aa;

            foreach (Key k; randomSamples) { aa[k] = true; }

            dln("D-AA: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            sleep(2);
            dln("Sleeping done");
        }

        dln();

        auto map = radixTreeMap!(Key, Value, radixPow2);
        assert(map.empty);
        static assert(map.isMap);

        map.insert(Key.init, Value.init);
    }
}

@safe pure nothrow /* TODO @nogc */
unittest
{
    check!(8,
           int, short, byte,
           uint, ushort, ubyte);
    // check!(4, ulong);
}

version(benchmark) unittest
{
    benchmark!8;
    // benchmark!4;
}

/** Static Iota.
    TODO Move to Phobos std.range.
 */
template iota(size_t from, size_t to)
    if (from <= to)
{
        alias iota = iotaImpl!(to - 1, from);
}
private template iotaImpl(size_t to, size_t now)
{
    import std.meta : AliasSeq;
    static if (now >= to) { alias iotaImpl = AliasSeq!(now); }
    else                  { alias iotaImpl = AliasSeq!(now, iotaImpl!(to, now + 1)); }
}
