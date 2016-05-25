/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree

    TODO Store `isKey` in top-most bit of length part of `IxsN prefix` in branch node.

    TODO Optimize PBr.findSub for specific `subCount`

    TODO Use bitset storage in PBr and call it PBr. Active when sizeof BrN
    is larger than 32 bytes (256 bits) and all leaves are single SLf. Converted to when BrN.length >= someLimit

    TODO Add function reprefix({PBr|FBr) and call after insertAt({PBr|FBr}). Only useful when one single leaf is present?
    TODO Is std.algorithm.countUntil the most suitable function to use in setSub(PBr*, ...)
    TODO Make array indexing/slicing as @trusted and use .ptr[] instead of [] when things are stable
    TODO Use std.experimental.allocator

    TODO - `set.prefix("alpha")`                           => `SortedTreeRange` of `Tuple!(string, Lang, PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha"))`                    => `SortedTreeRange` of `Tuple!(Lang, PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha", Lang.en))`           => `SortedTreeRange` of `Tuple!(PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha", Lang.en, PoT.noun))` => `SortedTreeRange` of `Tuple!(Sense)`.

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type FBr*?

    TODO Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`
    TODO Provide RandomAccess `opIndex` and `opSlice`! for variable-length keys aswell?
    TODO Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to _root. Add checks with `isSorted`.

    TODO Should opBinaryRight return void* instead of bool for set-case?
*/
module trie;

import std.traits : isIntegral, isFloatingPoint, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy, isPointer;
import std.typecons : tuple, Tuple, Unqual;
import std.range : isInputRange, ElementType;
import std.range.primitives : hasLength;

import bijections : isIntegralBijectableType, bijectToUnsigned;
import variant_ex : WordVariant;
import typecons_ex : IndexedArray, StrictlyIndexed;
import modulo : Mod, mod;

// version = debugAllocations;
version = benchmark;
// version = print;

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

/** Statically allocated `Ix`-array of fixed pre-allocated length `maxLength`.
 */
struct IxsN(size_t maxLength,
            uint radixPow2 = 8)
    if (maxLength >= 2)         // no use storing less than 2 bytes
{
    enum M = 2^^radixPow2;     // branch-multiplicity, typically either 2, 4, 16 or 256
    alias Ix = Mod!M;

    this(Ixs...)(Ixs ixs)
        if (Ixs.length >= 1 &&
            Ixs.length <= maxLength)
    {
        foreach (const i, const ix; ixs)
        {
            this.ixs[i] = ix;
        }
        this._length = ixs.length;
    }

    this(Ix[] ixs)
    {
        assert(ixs.length <= maxLength);
        this.ixs[0 .. ixs.length] = ixs;
        this._length = cast(ubyte)ixs.length;
    }

    @property auto toString() const
    {
        import std.conv : to;
        return ixs[0 .. _length].to!string;
    }

    @safe pure nothrow @nogc:

    bool empty() const { return _length == 0; }
    bool full() const { return _length == maxLength; }

    auto ref front() inout
    {
        assert(!empty);
        return ixs[0];
    }

    auto ref back() inout
    {
        assert(!empty);
        return ixs[_length - 1];
    }

    void popFront()
    {
        assert(!empty);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - 1)
        {
            ixs[i] = ixs[i + 1]; // TODO move construct?
        }
        --_length;
    }

    void popBack() { assert(!empty); --_length; }
    void pushBack(Ixs...)(Ixs moreIxs)
        if (Ixs.length <= maxLength)
    {
        assert(!full);
        foreach (const i, const ix; moreIxs)
        {
            this.ixs[_length + i] = ix;
        }
        _length += Ixs.length;
    }

    auto chunks() inout { return ixs[0 .. _length]; }
    alias chunks this;

    auto length() const { return _length; }

private:
    ubyte _length;               // number of defined elements in ixs
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

    auto x = IxsN!(maxLength, radixPow2)(ixs);
    auto y = IxsN!(maxLength, radixPow2)(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M);

    assert(x == y);

    assert(x.length == 4);
    assert(!x.empty);

    assert(x.equal([11, 22, 33, 44]));
    assert(!x.full);
    x.popFront;
    assert(x.equal([22, 33, 44]));
    assert(!x.full);
    x.popBack;
    assert(x.equal([22, 33]));
    assert(!x.full);
    x.popFront;
    assert(x.equal([33]));
    assert(!x.full);
    x.popFront;
    assert(x.empty);
    assert(!x.full);
    assert(x.length == 0);

    x.pushBack(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M, 55.mod!M, 66.mod!M, 77.mod!M);
    assert(x.equal([11, 22, 33, 44, 55, 66, 77]));
    assert(!x.empty);
    assert(x.full);
}

/** Returns: `true` if `r` and all `ss` all have equal length.
 */
bool equalLength(R, Ss...)(const R r, const Ss ss) @safe pure nothrow @nogc
    if (Ss.length >= 1 &&
        allSatisfy!(hasLength, R, Ss))
{
    foreach (const ref s; ss)
    {
        if (r.length != s.length) { return false; }
    }
    return true;
}

@safe pure nothrow unittest
{
    assert(equalLength([1], [2], [3]));
    assert(!equalLength([1, 1], [2], [3]));
    assert(!equalLength([1], [2, 2], [3]));
    assert(!equalLength([1], [2], [3, 3]));
}

/** Raw adaptive radix tree (ART) container storing untyped variable-length `Key`.

    In set-case (`Value` is `void`) this container is especially suitable for
    representing a set of 32 or 64 integers/pointers.

    Radix-trees are suitable for storing variable-keys and provide completion of
    all keys matching a given key prefix. This enables efficient storage of long
    URLs sharing a common prefix, typically a domain and path.

    Branch packing of leaves is more efficiently when `Key.sizeof` is fixed,
    that is `hasFixedKeyLength` returns `true`.

    For a good introduction to adaptive radix trees (ART) see also:
    https://infosys.cs.uni-saarland.de/publications/ARCD15.pdf

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree
    See also: https://github.com/npgall/concurrent-trees
    See also: http://code.dogmap.org/kart/
    See also: http://cr.yp.to/critbit.html
    See also: https://gcc.gnu.org/onlinedocs/libstdc++/ext/pb_ds/trie_based_containers.html
    See also: https://github.com/npgall/concurrent-trees
*/
private struct RawRadixTree(Value,
                            uint radixPow2 = 8) // binary power of radix, typically either 1, 2, 4 or 8
{
    import std.bitmanip : bitfields;
    import std.conv : to;
    import std.algorithm : filter;
    import std.meta : AliasSeq, staticMap;
    import std.typecons : ConstOf;
    import bitset : BitSet;

    static assert(radixPow2 == 8, "Radix is currently limited to 8");

    enum isSet = is(Value == void); // `true` if this tree is a set. TODO better to use empty struct?
    enum isMap = !isSet;            // `true` if this tree is a map

    enum radix = 2^^radixPow2;     // branch-multiplicity, typically either 2, 4, 16 or 256

    alias order = radix;   // tree order

    /// `true` if tree has binary branch.
    enum isBinary = radixPow2 == 2;

    /** Radix Modulo Index */
    alias Ix = Mod!radix; // restricted index type avoids range checking in array indexing below

    /** `radixPow2` least significant bits (LSB) of leaves directly packed into a word.

        TODO Generalize to packing of more than one `Ix` per byte.
        TODO respect byteorder in `MLf` to work with `WordVariant`
        TODO implement and use opSlice instead of .suffix[]
    */
    static if (size_t.sizeof == 8) // 64-bit CPU
    {
        static if (radixPow2 == 8)
        {
            /// Variable-Length Single-Key Leaf
            struct SLf
            {
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof;
                this(Ix[] suffix) { this.suffix = suffix; }
                IxsN!(maxLength, radixPow2) suffix;
                alias suffix this;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }

            /// Fixed-Length Multiple-Key Leaf
            struct MLf
            {
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof; // maximum number of elements
                Ix[maxLength] keys;
                ubyte length; // number of objects defined in keys
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }
        }
    }
    else
    {
        static assert(false, "Currently requires a 64-bit CPU (size_t.sizeof == 8)");
    }

    static if (radixPow2 == 8)
    {
        alias DefaultRootType = PBr*;
    }

    alias DefaultBr = DefaultRootType;
    alias DefaultLf = MLf;

    static if (isSet)
        static assert(MLf.sizeof == size_t.sizeof); // assert that it's size matches platform word-size

    /** Node types. */
    alias NodeTypes = AliasSeq!(SLf,
                                MLf,
                                PBr*,
                                BBr*,
                                FBr*);

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

    /** 256-Branch population histogram.
    */
    alias BBr_PopHist = size_t[radix];

    /** 4-Branch population histogram.
        Index maps to population with value range (1 .. 4).
    */
    alias PBr_PopHist = size_t[4];

    /** radix-Branch population histogram.
        Index maps to population with value range (1 .. `radix`).
    */
    alias FBr_PopHist = size_t[radix];

    /** radix-Leaf population histogram.
        Index maps to population with value range (1 .. `radix`).
    */
    alias LeafM_PopHist = size_t[radix];

    /** Tree Population and Memory-Usage Statistics. */
    struct Stats
    {
        BBr_PopHist popHist_BBr;
        PBr_PopHist popHist_PBr; // packed branch population histogram
        FBr_PopHist popHist_FBr; // full branch population histogram

        size_t allSLf0CountOfPBr; // number of `PBr` which sub-branches are all `SLf` of length 0
        size_t allSLf0CountOfFBr; // number of `FBr` which sub-branches are all `SLf` of length 0

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));
    }

    /** Full Bitset Branch with only bottom-most leaves. */
    static private struct BBr
    {
        enum prefixLength = 5;

        @safe pure nothrow:

        this(Ix[] prefix, bool isKey = false)
        {
            this.prefix = prefix;
            this.isKey = isKey;
        }

        pragma(inline) bool hasSubAt(Ix ix) const @nogc { return _keyBits[ix]; }
        pragma(inline) bool empty() const @nogc { return _keyBits.allZero; }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats)
        {
            const count = _keyBits.countOnes; // number of non-zero sub-nodes
            assert(count <= radix);
            ++stats.popHist_BBr[count - 1]; // TODO type-safe indexing
        }

        private:
        BitSet!radix _keyBits;  // 32 bytes
        IxsN!prefixLength prefix; // prefix common to all `subNodes` (also called edge-label)
        bool isKey;
    }

    static assert(BBr.sizeof == 40);

    /** Sparse/Packed/Partial 4-way branch. */
    static private struct PBr
    {
        enum N = 4;

        enum prefixLength = 7;

        @safe pure nothrow:

        this(Ix[] prefix, bool isKey = false)
        {
            this.prefix = prefix;
            this.isKey = isKey;
        }

        this(Ix[] prefix, bool isKey, Ix subIx, Node subNonSLf)
        {
            this.prefix = prefix;
            this.isKey = isKey;
            this.subIxSlots.at!0 = subIx;
            this.subNodeSlots.at!0 = subNonSLf;
            this.subCount = 1;
        }

        void pushBackSub(Tuple!(Ix, Node) sub)
        {
            assert(!full);
            const backIx = subCount.mod!N;
            subIxSlots[backIx] = sub[0];
            subNodeSlots[backIx] = sub[1];
            subCount = cast(ubyte)(subCount + 1);
        }
        inout(Node) findSub(Ix ix) inout
        {
            switch (subCount)
            {
            case 0:
                break;
            case 1:
                if (subIxSlots.at!0 == ix) { return subNodeSlots.at!0; }
                break;
            case 2:
                foreach (i; iota!(0, 2))
                {
                    if (subIxSlots.at!i == ix) { return subNodeSlots.at!i; }
                }
                break;
            default:
                // TODO do binary search
                foreach (const i_; 0 ..  subCount)
                {
                    const i = i_.mod!N;
                    if (subIxSlots[i] == ix) { return subNodeSlots[i]; }
                }
                break;
            }
            return Node.init;
        }

        pragma(inline) bool empty() const @nogc { return subCount == 0; }
        pragma(inline) bool full() const @nogc { return subCount == N; }

        pragma(inline) auto subNodes() inout @nogc
        {
            return subNodeSlots[0 .. subCount];
        }

        /** Returns `true` if this branch can be packed into a bitset, that is
            contains only sub-nodes of type `SLf` of zero length. */
        bool isBitPackable() const @nogc
        {
            typeof(return) allSLf0 = true;
            foreach (const sub; subNodes)
            {
                if (const subSLfRef = sub.peek!SLf)
                {
                    const subSLf = *subSLfRef;
                    if (subSLf.length != 0) { allSLf0 = false; }
                }
            }
            return allSLf0;
        }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) const
        {
            size_t count = 0; // number of non-zero sub-nodes
            foreach (const sub; subNodes)
            {
                ++count;
                sub.calculate!(Value, radixPow2)(stats);
            }
            assert(count <= radix);
            ++stats.popHist_PBr[count - 1]; // TODO type-safe indexing
            stats.allSLf0CountOfPBr += isBitPackable;
        }

        private:

        // members in order of decreasing `alignof`:
        StrictlyIndexed!(Node[N]) subNodeSlots;
        IxsN!prefixLength prefix; // prefix common to all `subNodes` (also called edge-label)
        StrictlyIndexed!(Ix[N]) subIxSlots;
        mixin(bitfields!(ubyte, "subCount", 7, // counts length of defined elements in subNodeSlots
                         bool, "isKey", 1)); // key at this branch is occupied
    }

    static assert(PBr.sizeof == 48);

    enum brMPrefixLength = 15; // we can afford larger prefix here because FBr is so large

    /** Dense/Unpacked `radix`-branch with `radix` number of sub-nodes. */
    static private struct FBr
    {
        @safe pure nothrow:

        this(Ix[] prefix, bool isKey = false)
        {
            this.prefix = prefix;
            this.isKey = isKey;
        }

        this(PBr* rhs)
        {
            this.prefix = rhs.prefix;
            this.isKey = rhs.isKey;
            foreach (i; 0 .. rhs.subCount) // each sub node. TODO use iota!(Mod!N)
            {
                const iN = i.mod!(PBr.N);
                const ix = rhs.subIxSlots[iN];
                this.subNodes[ix] = rhs.subNodes[iN];
            }
        }

        IxsN!brMPrefixLength prefix; // prefix (edge-label) common to all `subNodes`
        bool isKey;      // key at this branch is occupied
        StrictlyIndexed!(Node[radix]) subNodes;

        /** Returns `true` if this branch can be packed into a bitset, that is
            contains only subNodes of type `SLf` of zero length. */
        bool isBitPackable() const @safe pure nothrow @nogc
        {
            typeof(return) allSLf0 = true;
            foreach (const subNonSLf; subNodes)
            {
                if (const subSLfRef = subNonSLf.peek!SLf)
                {
                    if ((*subSLfRef).length != 0) { allSLf0 = false; }
                }
            }
            return allSLf0;
        }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats)  /* TODO @nogc */ const
        {
            size_t count = 0; // number of non-zero sub-nodes
            foreach (const subNonSLf; subNodes)
            {
                if (subNonSLf)
                {
                    ++count;
                    subNonSLf.calculate!(Value, radixPow2)(stats);
                }
            }
            assert(count <= radix);
            ++stats.popHist_FBr[count - 1]; // TODO type-safe indexing
            stats.allSLf0CountOfFBr += isBitPackable;
        }
    }

    static if (false)
    {
        pragma(msg, "PBr.sizeof:", PBr.sizeof, " PBr.alignof:", PBr.alignof);
        pragma(msg, "PBr.subNodes.sizeof:", PBr.subNodes.sizeof, " PBr.subNodes.alignof:", PBr.subNodes.alignof);
        pragma(msg, "PBr.prefix.sizeof:", PBr.prefix.sizeof, " PBr.prefix.alignof:", PBr.prefix.alignof);
        pragma(msg, "PBr.subIxs.sizeof:", PBr.subIxs.sizeof, " PBr.subIxs.alignof:", PBr.subIxs.alignof);
    }

    /** Set sub-`Node` of branch `Node curr` at index `ix` to `subNonSLf`. */
    pragma(inline) Node setSub(Node curr, Ix subIx, Node subNonSLf)
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_BBrPtr: return setSub(curr.as!(BBr*), subIx, subNonSLf);
        case Node.Ix.ix_PBrPtr: return setSub(curr.as!(PBr*), subIx, subNonSLf);
        case Node.Ix.ix_FBrPtr: return setSub(curr.as!(FBr*), subIx, subNonSLf);
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }
    /// ditto
    Node setSub(BBr* curr, Ix subIx, Node subNonSLf) @safe pure nothrow /* TODO @nogc */
    {
        if (const subSLfRef = subNonSLf.peek!(SLf))
        {
            const subSLf = *subSLfRef;
            if (subSLf.suffix.empty)
            {
                curr._keyBits[subIx] = true;
                freeNode(subNonSLf); // free it because it's stored inside the bitset itself
                return Node(curr);
            }
            else
            {
            }
        }
        const currValue = *curr;
        show!currValue;
        show!subIx;
        show!subNonSLf;
        assert(false, "Expand BBr into either PBr or FBr depending on curr.popcnt");
    }
    /// ditto
    Node setSub(PBr* curr, Ix subIx, Node subNonSLf) @safe pure nothrow /* TODO @nogc */
    {
        import std.algorithm : countUntil;
        const i = curr.subIxSlots[0 .. curr.subCount].countUntil(subIx); // TODO is this the preferred function?
        if (i != -1)            // if hit. TODO use bool conversion if this gets added to countUntil
        {
            curr.subNodeSlots[i.mod!(curr.N)] = subNonSLf; // reuse
        }
        else if (!curr.full)     // if room left in curr
        {
            curr.pushBackSub(tuple(subIx, subNonSLf)); // add one to existing
        }
        else
        {
            return setSub(expand(curr), subIx, subNonSLf); // fast, because directly calls setSub(FBr*, ...)
        }
        return Node(curr);
    }
    /// ditto
    pragma(inline) Node setSub(FBr* curr, Ix subIx, Node subNonSLf) @safe pure nothrow /* TODO @nogc */
    {
        curr.subNodes[subIx] = subNonSLf;
        return Node(curr);
    }

    /** Get sub-`Node` of branch `Node br` at index `ix. */
    inout(Node) getSub(inout Node br, Ix ix) @safe pure nothrow
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_BBrPtr:
            if (br.as!(BBr*).hasSubAt(ix))
            {
                return Node(SLf.init);
            }
            break;
        case Node.Ix.ix_PBrPtr:
            if (auto subNonSLf = br.as!(PBr*).findSub(ix))
            {
                return subNonSLf;
            }
            break;
        case Node.Ix.ix_FBrPtr:
            return br.as!(FBr*).subNodes[ix];
        default:
            assert(false, "Unsupported Node type " ~ br.typeIx.to!string);
        }
        return Node.init;
    }

    /** Returns: `true` if `br` is occupied, `false` otherwise. */
    pragma(inline) bool isKey(Node br) const @safe pure nothrow
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_BBrPtr: return br.as!(BBr*).isKey;
        case Node.Ix.ix_PBrPtr: return br.as!(PBr*).isKey;
        case Node.Ix.ix_FBrPtr: return br.as!(FBr*).isKey;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ br.typeIx.to!string);
        }
    }

    pragma(inline) void makeKey(Node br) const @safe pure nothrow
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_BBrPtr: br.as!(BBr*).isKey = true; break;
        case Node.Ix.ix_PBrPtr: br.as!(PBr*).isKey = true; break;
        case Node.Ix.ix_FBrPtr: br.as!(FBr*).isKey = true; break;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ br.typeIx.to!string);
        }
    }

    /** Get prefix of branch node `br`. */
    auto getPrefix(inout Node br) @safe pure nothrow
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_BBrPtr: return br.as!(BBr*).prefix[];
        case Node.Ix.ix_PBrPtr: return br.as!(PBr*).prefix[];
        case Node.Ix.ix_FBrPtr: return br.as!(FBr*).prefix[];
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ br.typeIx.to!string);
        }
    }

    /** Set prefix of branch node `br` to `prefix`. */
    void setPrefix(Node br, Ix[] prefix) @safe pure nothrow
    {
        switch (br.typeIx)
        {
        case Node.Ix.ix_BBrPtr: br.as!(BBr*).prefix = typeof(br.as!(BBr*).prefix)(prefix); break;
        case Node.Ix.ix_PBrPtr: br.as!(PBr*).prefix = typeof(br.as!(PBr*).prefix)(prefix); break;
        case Node.Ix.ix_FBrPtr: br.as!(FBr*).prefix = typeof(br.as!(FBr*).prefix)(prefix); break;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ br.typeIx.to!string);
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
        assert(_branchCount == 0, "Pointer node count is not zero, but " ~ _branchCount.to!string);
    }

    @safe pure nothrow /* TODO @nogc */
    {
        /** Insert `key` into `this` tree. */
        pragma(inline) Node insert(Key!radixPow2 key, out bool wasAdded)
        {
            return _root = insertAt(_root, key, 0, wasAdded);
        }

        Node insertNew(Key!radixPow2 key, size_t superPrefixLength, out bool wasAdded)
        {
            if (key.length <= SLf.maxLength)
            {
                SLf currSLf = construct!(SLf)(key);
                wasAdded = true;
                return Node(currSLf);
            }
            else                // key doesn't fit in a `SLf`
            {
                import std.algorithm : min;
                auto brKey = key[0 .. min(key.length, DefaultBr.prefixLength)];
                return insertAt(Node(construct!(DefaultBr)(brKey, false)), // as much as possible of key in branch prefix
                                key, superPrefixLength, wasAdded);
            }
        }

        /** Insert `key` into sub-tree under root `curr`. */
        pragma(inline) Node insertAt(Node curr, Key!radixPow2 key, size_t superPrefixLength, out bool wasAdded)
        {
            if (!curr)          // if no existing `Node` to insert at
            {
                curr = insertNew(key, superPrefixLength, wasAdded);
                assert(wasAdded); // must be added to new Node
                return curr;
            }
            else
            {
                with (Node.Ix)
                {
                    final switch (curr.typeIx)
                    {
                    case undefined: break;
                    case ix_SLf:    return insertAt(curr.as!(SLf), key, superPrefixLength, wasAdded);
                    case ix_MLf:   return insertAt(curr.as!(MLf), key, superPrefixLength, wasAdded);

                    case ix_BBrPtr:
                    case ix_PBrPtr:
                    case ix_FBrPtr: return insertAtBranch(curr, key, superPrefixLength, wasAdded);
                    }
                    assert(false);
                }
            }
        }

        Node insertAtBranch(Node curr, Key!radixPow2 key, size_t superPrefixLength, out bool wasAdded)
        {
            import std.algorithm : commonPrefix;
            auto currPrefix = getPrefix(curr);
            auto matchedPrefix = commonPrefix(key, currPrefix);

            // prefix:"abcd", key:"ab"
            if (matchedPrefix.length == key.length &&
                matchedPrefix.length < currPrefix.length) // prefix is an extension of key
            {
                const subIx = currPrefix[matchedPrefix.length]; // need index first
                setPrefix(curr, currPrefix[matchedPrefix.length + 1 .. $]); // drop matchedPrefix plus index
                return Node(construct!(DefaultBr)(matchedPrefix, true, // `true` because `key` occupies this node
                                                  subIx, curr));
            }
            // prefix:"ab", key:"abcd"
            else if (matchedPrefix.length == currPrefix.length &&
                     matchedPrefix.length < key.length) // key is an extension of prefix
            {
                key = key[matchedPrefix.length .. $]; // strip `currPrefix from beginning of `key`
                superPrefixLength += matchedPrefix.length;
                // continue below
            }
            // prefix:"ab", key:"ab"
            else if (matchedPrefix.length == currPrefix.length && // exact key prefix match
                     matchedPrefix.length == key.length)
            {
                if (!isKey(curr))
                {
                    makeKey(curr);
                    wasAdded = true;
                }
                return curr;
            }
            // prefix:"ab", key:"cd"
            else if (matchedPrefix.length == 0) // no prefix key match
            {
                if (currPrefix.length == 0) // no current prefix
                {
                    // continue below
                }
                else
                {
                    const subIx = currPrefix[0]; // subIx = 'a'
                    setPrefix(curr, currPrefix[1 .. $].to!(typeof(DefaultBr.prefix))); // new prefix becomes "b"
                    return insertAt(Node(construct!(DefaultBr)(Ix[].init, false, subIx, curr)),
                                    key,
                                    superPrefixLength,
                                    wasAdded);
                }
            }

            const ix = key[0];
            return setSub(curr, ix,
                          insertAt(getSub(curr, ix), // recurse
                                   key[1 .. $],
                                   superPrefixLength + 1,
                                   wasAdded));
        }

        Node insertAt(SLf curr, Key!radixPow2 key, size_t superPrefixLength, out bool wasAdded)
        {
            import std.range : empty;
            import std.algorithm : commonPrefix;

            if (key.empty)
            {
                assert(curr.suffix.empty, "Leaf is not empty when key is");
                return Node(curr);
            }

            auto matchedPrefix = commonPrefix(key, curr.suffix);
            if (equalLength(matchedPrefix, key, curr.suffix)) // key already stored
            {
                return Node(curr); // already stored in `curr`
            }
            else
            {
                return insertAt(split(curr, matchedPrefix, key),
                                key, superPrefixLength, wasAdded);
            }
        }

        Node insertAt(MLf curr, Key!radixPow2 key, size_t superPrefixLength, out bool wasAdded)
        {
            const Ix ix = key[0];

            // TODO this is only marginally faster:
            // foreach (const i; iota!(0, curr.maxLength))
            // {
            //     if (i == curr.length) break;
            //     else if (curr.keys[i] == ix) { return Node(curr); }
            // }

            import std.algorithm.searching : canFind;
            if (curr.keys[0 .. curr.length].canFind(ix)) // if already stored. TODO use binarySearch
            {
                return Node(curr); // already there, so return current node as is
            }

            if (curr.length < curr.maxLength) // if room left
            {
                curr.keys[curr.length++] = ix;
                // import sortn : networkSortUpTo;
                // TODO curr.keys[0 .. length].networkSort;
                // TODO curr.keys[0 .. length].sort;
                wasAdded = true;
                return Node(curr); // current node still ok
            }
            else
            {
                return insertAt(Node(expand(curr)), key, superPrefixLength, wasAdded); // NOTE stay at same (depth)
            }
        }

        /** Split `curr` using `prefix`. */
        Node split(SLf curr, Key!radixPow2 prefix, Key!radixPow2 key) // TODO key here is a bit malplaced
        {
            Node next;
            if (curr.length == 1 && key.length == 1) // storage in outer node is possible
            {
                if (prefix.length == 0)
                {
                    return Node(construct!(MLf)(curr[0]));
                }
                else if (prefix.length == 1)
                {
                    assert(false, "Use P1Lf with single-length prefix and a maximum of 4 ");
                }
                else
                {
                    next = construct!(BBr*)(prefix, false);
                }
            }
            if (!next)
            {
                next = construct!(DefaultBr)(prefix, false);
            }

            bool wasAdded;      // dummy
            auto node = insertAt(next, curr.suffix, 0, wasAdded);
            assert(wasAdded); // assure that existing key was reinserted
            freeNode(curr);   // remove old current

            return node;
        }

        /** Construct and return sub-Node at `key`.  */
        Node constructSub(Key!radixPow2 key)
        {
            import std.range : empty;
            if (key.empty)
            {
                return Node(construct!(SLf));
            }
            else
            {
                const bool isLast = key.length == 1;
                return (isLast ?
                        Node(construct!(DefaultLf)) :
                        Node(construct!(DefaultBr)));
            }
        }

        /** Destructively expand `curr` of type `PBr` into a `FBr` and return it. */
        FBr* expand(PBr* curr)
        {
            auto next = construct!(typeof(return))(curr);
            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` into a `BBr` and return it. */
        BBr* expand(MLf curr)
        {
            auto next = construct!(typeof(return));
            foreach (const ixM; curr.keys[0 .. curr.length])
            {
                assert(!next._keyBits[ixM]); // assert no duplicates in keys
                next._keyBits[ixM] = true;
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
        version(debugAllocations) { dln("constructing ", U.stringof, " from ", args); }
        static if (isPointer!U)
        {
            debug ++_branchCount;
            import std.conv : emplace;
            return emplace(cast(U)malloc((*U.init).sizeof), args);
            // TODO ensure alignment of node at least that of U.alignof
        }
        else
        {
            return U(args);
        }
    }

    void freeNode(NodeType)(NodeType nt) @trusted
    {
        version(debugAllocations) { dln("freeing ", NodeType.stringof, " ", nt); }
        static if (isPointer!NodeType)
        {
            free(cast(void*)nt);  // TODO Allocator.free
            debug --_branchCount;
        }
    }

    @safe pure nothrow /* TODO @nogc */
    {
        void release(BBr* curr)
        {
            freeNode(curr);
        }

        void release(PBr* curr)
        {
            foreach (sub; curr.subNodes[0 .. curr.subCount])
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(FBr* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(SLf curr) { freeNode(curr); }
        void release(MLf curr) { freeNode(curr); }

        void release(Node curr)
        {
            version(debugAllocations) { dln("releasing Node ", curr); }
            with (Node.Ix)
            {
                final switch (curr.typeIx)
                {
                case undefined: break; // ignored
                case ix_SLf: return release(curr.as!(SLf));
                case ix_MLf: return release(curr.as!(MLf));
                case ix_BBrPtr: return release(curr.as!(BBr*));
                case ix_PBrPtr: return release(curr.as!(PBr*));
                case ix_FBrPtr: return release(curr.as!(FBr*));
                }
            }
        }
    }

    /** Ensure that root `Node` is allocated. */
    void ensureRootNode(U = DefaultRootType)()
    {
        if (!_root) { _root = construct!(U); }
    }

    /** Get fixed length of keys, or `size_t.max` if key length is variable. */
    size_t keyLength() const @safe pure nothrow @nogc { return _keyLength; }

    /** Returns: `true` if all keys are of fixed size, `false` otherwise. */
    bool hasFixedKeyLength() const @safe pure nothrow @nogc { return keyLength != size_t.max; }

    /// Returns: number of nodes used in `this` tree.
    pragma(inline) debug size_t branchCount() @safe pure nothrow /* TODO @nogc */ { return _branchCount; }

    void print() @safe const
    {
        printAt(_root, 0);
    }

    void printAt(Node curr, size_t depth) @safe const
    {
        import std.range : repeat;
        import std.stdio : write, writeln;

        if (!curr) { return; }

        foreach (const i; 0 .. depth)
        {
            write(depth);
        }

        write("-");

        with (Node.Ix)
        {
            import std.algorithm : map;
            final switch (curr.typeIx)
            {
            case undefined: break;
            case ix_SLf:
                auto currSLf = curr.as!(SLf);
                writeln(typeof(currSLf).stringof, "#", currSLf.length, ": ", currSLf[]);
                break;
            case ix_MLf:
                auto currMLf = curr.as!(MLf);
                writeln(typeof(currMLf).stringof, "#", currMLf.length, ": ", currMLf.keys[0 .. currMLf.length]);
                break;
            case ix_BBrPtr:
                auto currBBr = curr.as!(BBr*);
                write(typeof(*currBBr).stringof, ":");
                if (!currBBr.prefix.empty) { write(" prefix=", currBBr.prefix); }
                write(" #ones=", currBBr._keyBits.countOnes);
                writeln;
                break;
            case ix_PBrPtr:
                auto currPBr = curr.as!(PBr*);
                write(typeof(*currPBr).stringof, ": ");
                if (!currPBr.prefix.empty) { write(" prefix=", currPBr.prefix); }

                // print sub-leaves
                write("sub-SLf-lengths=");
                foreach (const subSLf; currPBr.subNodes[].map!(subNonSLf => subNonSLf.peek!SLf))
                {
                    if (subSLf)
                    {
                        write((*subSLf).length);
                    }
                }
                writeln;

                // print sub-branches
                foreach (const subNonSLf; currPBr.subNodes[].filter!(subNonSLf => !subNonSLf.peek!SLf))
                {
                    printAt(subNonSLf, depth + 1);
                }
                break;
            case ix_FBrPtr:
                auto currFBr = curr.as!(FBr*);
                write(typeof(*currFBr).stringof, ": ");
                if (!currFBr.prefix.empty) { write(" prefix=", currFBr.prefix); }

                // print sub-leaves
                write("sub-SLf-lengths=");
                foreach (const subSLf; currFBr.subNodes[].map!(subNonSLf => subNonSLf.peek!SLf))
                {
                    if (subSLf)
                    {
                        write((*subSLf).length);
                    }
                }
                writeln;

                // print sub-branches
                foreach (const subNonSLf; currFBr.subNodes[].filter!(subNonSLf => !subNonSLf.peek!SLf))
                {
                    printAt(subNonSLf, depth + 1);
                }

                break;
            }
        }
    }

    private:
    Node _root;                 ///< tree root node
    size_t _length = 0; ///< number of elements (keys or key-value-pairs) currently stored under `_root`
    immutable _keyLength = size_t.max; ///< maximum length of key
    debug long _branchCount = 0;
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
        case ix_SLf: break; // TODO calculate()
        case ix_MLf: break; // TODO calculate()
        case ix_BBrPtr: sub.as!(RT.BBr*).calculate(stats); break;
        case ix_PBrPtr: sub.as!(RT.PBr*).calculate(stats); break;
        case ix_FBrPtr: sub.as!(RT.FBr*).calculate(stats); break;
        }
    }
}

/// Radix-Tree with key-type `Key` and value-type `Value`.
struct RadixTree(Key, Value, uint radixPow2 = 8)
    if (allSatisfy!(isTrieableKeyType, Key))
{
    this(bool unusedDummy)      // TODO how do we get rid of the need for `unusedDummy`?
    {
        this._keyLength = isFixedTrieableKeyType!Key ? Key.sizeof : size_t.max;
    }

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

            enum nbits = 8*ukey.sizeof; // bitsize of ukey
            enum chunkCount = nbits/radixPow2; // number of chunks in ukey
            static assert(chunkCount*radixPow2 == nbits, "Bitsize of Key must be a multiple of radixPow2:" ~ radixPow2.stringof);

            KeyN!(radixPow2, Key.sizeof) key;

            static if (radixPow2 == 8)
            {
                foreach (bix; 0 .. chunkCount)
                {
                    const bitShift = (chunkCount - 1 - bix)*radixPow2; // most significant bit chunk first (MSBCF)
                    key[bix] = (ukey >> bitShift) & (radix - 1); // part of value which is also an index
                }
            }
        }
        else static if (is(Unqual!Key == string))
        {
            immutable ubyte[] key = Key.representation; // lexical byte-order
        }
        else static if (is(Unqual!Key == wstring))
        {
            immutable ushort[] rKey = Key.representation; // lexical byte-order. TODO do we need most significant byte byte-order for each `ushort` for this to work?
            immutable ubyte[] key = (cast(immutable ubyte*)rKey.ptr)[0 .. rKey[0].sizeof * rKey.length]; // TODO @trusted functionize. Reuse existing Phobos function?
        }
        else static if (is(Unqual!Key == dstring))
        {
            immutable uint[] rKey = Key.representation; // lexical byte-order. TODO do we need most significant byte byte-order for each `ushort` for this to work?
            immutable ubyte[] key = (cast(immutable ubyte*)rKey.ptr)[0 .. rKey[0].sizeof * rKey.length]; // TODO @trusted functionize. Reuse existing Phobos function?
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

    void print() @safe const
    {
        _tree.print();
    }

    private RawRadixTree!(Value, radixPow2) _tree;
    alias _tree this;
}
alias PatriciaTrie = RadixTree;
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator of set-version of `RadixTree` where value-type is `void` (unused).
auto radixTreeSet(Key, uint radixPow2 = 4)() { return RadixTree!(Key, void, radixPow2)(false); }

/// Instantiator of map-version of `RadixTree` where value-type is `Value`.
auto radixTreeMap(Key, Value, uint radixPow2 = 4)() { return RadixTree!(Key, Value, radixPow2)(false); }

@safe pure nothrow /* TODO @nogc */ unittest
{
    enum radixPow2 = 8;
    auto set = radixTreeSet!(ushort, radixPow2);

    assert(set.insert(0));
    assert(!set.insert(0));
    assert(set.branchCount == 0);

    foreach (const i; 1 .. 256)
    {
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.branchCount == 1);
    }

    assert(set.insert(256));
    assert(!set.insert(256));
    assert(set.branchCount == 2);

    assert(set.insert(257));
    assert(!set.insert(257));
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
            assert(set.hasFixedKeyLength == isFixedTrieableKeyType!Key);
            assert(set.empty);

            static assert(set.isSet);

            import std.algorithm : min, max;

            static if (isIntegral!Key)
            {
                const low = max(Key.min, -100_000);
                const high = min(Key.max, 100_000);
            }
            else static if (isFloatingPoint!Key)
            {
                const low = -100_000;
                const high = 100_000;
            }
            const length = high - low + 1;

            const useContains = false;
            size_t cnt = 0;
            foreach (const uk; low.iota(high + 1))
            {
                const Key key = cast(Key)uk;
                // dln("key:", key);
                if (useContains)
                {
                    assert(!set.contains(key)); // key should not yet be in set
                    assert(key !in set);        // alternative syntax
                }

                assert(set.insert(key));  // insert new value returns `true` (previously not in set)

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
            assert(map.hasFixedKeyLength == isFixedTrieableKeyType!Key);
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
            dln("Sparse Bit-Branch Population Histogram: ", stats.popHist_BBr);
            dln("Sparse 4-Branch Population Histogram: ", stats.popHist_PBr);
            dln("Dense radix=", 2^^radixPow2, "-Branch Population Histogram: ", stats.popHist_FBr);
            dln("Population By Node Type: ", stats.popByNodeType);
            dln("Number of PBr with SLf-0 only subNodes: ", stats.allSLf0CountOfPBr);
            dln("Number of FBr with SLf-0 only subNodes: ", stats.allSLf0CountOfFBr);

            size_t totalBytesUsed = 0;
            foreach (Set.Node.Ix ix, pop; stats.popByNodeType) // TODO use stats.byPair when added to typecons_ex.d
            {
                size_t bytesUsed = 0;
                with (Set.Node.Ix)
                {
                    final switch (ix)
                    {
                    case undefined: break;
                    case ix_SLf:    bytesUsed = pop*Set.SLf.sizeof; break;
                    case ix_MLf:   bytesUsed = pop*Set.MLf.sizeof; break;
                    case ix_BBrPtr: bytesUsed = pop*Set.BBr.sizeof; totalBytesUsed += bytesUsed; break;
                    case ix_PBrPtr: bytesUsed = pop*Set.PBr.sizeof; totalBytesUsed += bytesUsed; break;
                    case ix_FBrPtr: bytesUsed = pop*Set.FBr.sizeof; totalBytesUsed += bytesUsed; break;
                    }
                }
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

        // version(print)
        set.print();

        auto map = radixTreeMap!(Key, Value, radixPow2);
        assert(map.empty);
        static assert(map.isMap);

        map.insert(Key.init, Value.init);
    }
}

@safe pure nothrow /* TODO @nogc */
unittest
{
    // TODO Support this struct A { long x, y; }
    check!(8,
           byte, short, int, long,
           ubyte, ushort, uint, ulong,
           float, double,
        );
}

auto testPrint(uint radixPow2, Keys...)()
    if (Keys.length >= 1)
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        foreach (Key; Keys)
        {
            dln("Key: ", Key.stringof);
            alias Tree = radixTreeSet!(Key, radixPow2);
            auto set = Tree;

            import std.algorithm : min, max;

            static if (isIntegral!Key)
            {
                const low = max(Key.min, -100_000);
                const high = min(Key.max, 100_000);
            }
            else static if (isFloatingPoint!Key)
            {
                const low = -100_000;
                const high = 100_000;
            }

            foreach (const uk; low.iota(high + 1))
            {
                const Key key = cast(Key)uk;
                assert(set.insert(key));  // insert new value returns `true` (previously not in set)
                assert(!set.insert(key)); // reinsert same value returns `false` (already in set)
            }

            version(print) set.print();
        }
    }
}

version(print) @safe unittest
{
    testPrint!(8,
               byte, short, int, long,
               ubyte, ushort, uint, ulong,
               float, double,
        );
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
