# phobos-next
Various reusable D code.

Most definitions are probably generic enough to be part of Phobos.

See also: http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org

- trie.d: Tries and Prefix Trees, both set and map case. Supports the following D interfaces
  - ForwardRange access via `opSlice`: `foreach (e; tree[]) {}`
  - Insertion: `set.insert(Key key)`
  - `contains()`
  - `opIndex`
  - `opIndexAssign`
  - Prefix Completion (via Range)

- array_ex.d
  - pushBack
