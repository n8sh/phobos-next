/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO reuse `UnsignedOfSameSizeAs`
    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isArray, allSatisfy, anySatisfy;
import std.range : ElementType;

enum isTrieableKeyElementType(T) = isIntegral!T || isSomeChar!T;

enum isTrieableKey(T) = (isTrieableKeyElementType!T ||
                         (isArray!T &&
                          isTrieableKeyElementType!(ElementType!T)));

struct Node(size_t N)
{
    Node!N*[N] nexts;

    /// Indicates that only child at this index is occupied.
    static Node!N* oneSet = cast(Node!N*)1;

    /// Indicates that all children are occupied (typically only for fixed-sized types).
    static Node!N* allSet = cast(Node!N*)size_t.max;
}

/** Radix Tree storing keys of type `Key`.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Key, Value)
    if (allSatisfy!(isTrieableKey, Key))
{
    enum isSet = is(Value == void);
    enum hasValue = !isSet;

    enum radix = 4;             // radix in number of bits
    enum N = 2^^radix;

    /// Check if tree has fixed depth.
    enum isFixed = isTrieableKeyElementType!Key;

    enum isBinary = radix == 2;

    static if (isSet)
    {
        void insert(Key key)
        {
            auto current = _root;
        }
        bool contains(Key key) const
        {
            return true;
        }
    }
    else
    {
        void insert(Key key, Value value)
        {
        }
        Value contains(Key key) const
        {
            return Value.init;
        }
    }

    private Node!N* _root;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator.
auto radixTreeMap(Key, Value)() { return RadixTree!(Key, Value)(); }

/// Instantiator.
auto radixTreeSet(Key)() { return RadixTree!(Key, void)(); }

@safe pure nothrow unittest
{
    struct X { int i; float f; string s; }
    alias Value = X;
    foreach (Key; AliasSeq!(char, uint, string))
    {
        auto set = radixTreeSet!(Key);
        set.insert(Key.init);
        assert(set.contains(Key.init));

        auto map = radixTreeMap!(Key, Value);
        map.insert(Key.init, Value.init);
    }
}
