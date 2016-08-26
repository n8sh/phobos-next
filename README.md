# phobos-next
Various reusable D code.

Most definitions are probably generic enough to be part of Phobos.

See also: http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org

- trie.d: Tries and Prefix Trees Containers, both set and map case. Supports the following D interfaces
  - ForwardRange access via `opSlice`: `foreach (e; tree[]) {}`
  - Insertion: `set.insert(Key key)`
  - `contains()`
  - `opIndex`
  - `opIndexAssign`
  - Prefix Completion (via Range)
  - See `https://github.com/nordlow/phobos-next/blob/master/src/test_trie_prefix.d` for a descriptive usage of prefixed access.

- array_ex.d: Array container(s) with optional sortedness (`Ordering`).
  - pushBack
