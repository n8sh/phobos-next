# phobos-next
Various reusable D code.

Most definitions are probably generic enough to be part of Phobos.

See also: http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org

- trie.d: Tries and Prefix Trees Containers, both set and map case. API:

- array_ex.d: Array container(s) with optional sortedness (`Ordering`). API:
  - Always @safe pure nothrow @nogc when possible
  - Currently value copy semantics only
  - pushBack
