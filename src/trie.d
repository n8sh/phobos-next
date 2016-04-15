/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie
 */
module trie;

import std.traits : isIntegral, isSomeString;
import std.range : ElementType;

enum isTrieableElementType(T) = isIntegral!T || isSomeChar!T;
enum isTrieType(T) = (isTrieableElementType ||
                      (isArray!T &&
                       isTrieableElementType!(ElementType!T)));

/** Radix Tree.
    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Value = void, Keys...)
{

    bool contains(T) const;
}

@safe pure nothrow unittest
{
}
