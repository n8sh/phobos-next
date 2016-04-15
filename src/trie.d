/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse
 */
module trie;

import std.meta : AliasSeq;
import std.traits : isIntegral, isSomeChar, isSomeString, isArray, allSatisfy;
import std.range : ElementType;

enum isTrieableKeyElementType(T) = isIntegral!T || isSomeChar!T;

enum isTrieableKey(T) = (isTrieableKeyElementType!T ||
                         (isArray!T &&
                          isTrieableKeyElementType!(ElementType!T)));

/** Radix Tree storing keys of types `Keys`.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Value, Keys...)
    if (allSatisfy!(isTrieableKey, Keys))
{
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator.
auto radixTreeMap(Value, Keys...)()
{
    return RadixTree!(Value, Keys)();
}

/// Instantiator.
auto radixTreeSet(Keys...)()
{
    return RadixTree!(void, Keys)();
}

@safe pure nothrow unittest
{
    struct S { int i; float f; string s; }
    alias Value = S;
    foreach (Key; AliasSeq!(uint, char, string))
    {
        auto set = radixTreeSet!Key();
        set.insert(Key.init);

        auto map = radixTreeMap!(Value, Key)();
        set.insert(Key.init, Value.init);
    }
}
