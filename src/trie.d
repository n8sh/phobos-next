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

    /// Indicates that child at this index is occupied.
    static Node!N* oneSet = cast(Node!N*)1;

    /// Indicates that all children are occupied (typically only for fixed-sized types).
    static Node!N* allSet = cast(Node!N*)size_t.max;
}

/** Radix Tree storing keys of types `Keys`.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Value, Keys...)
    if (allSatisfy!(isTrieableKey, Keys))
{
    enum isSet = is(Value == void);
    enum hasValue = !isSet;

    enum radix = 4;             // radix in number of bits
    enum N = 2^^radix;

    enum isFixed = allSatisfy!(isTrieableKeyElementType, Keys);

    static if (isSet)
    {
        void insert(Keys keys)
        {
        }
        bool contains(Keys keys) const
        {
            return true;
        }
    }
    else
    {
        void insert(Value value, Keys keys)
        {
        }
        Value contains(Keys keys) const
        {
            return Value.init;
        }
    }

    private Node!N* _root;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator.
auto radixTreeMap(Value, Keys...)() { return RadixTree!(Value, Keys)(); }

/// Instantiator.
auto radixTreeSet(Keys...)() { return RadixTree!(void, Keys)(); }

@safe pure nothrow unittest
{
    struct X { int i; float f; string s; }
    alias Value = X;
    foreach (Key; AliasSeq!(char, uint, string))
    {
        auto set = radixTreeSet!(Key);
        set.insert(Key.init);
        assert(set.contains(Key.init));

        auto map = radixTreeMap!(Value, Key);
        map.insert(Value.init, Key.init);
    }
}
