# phobos-next

Various reusable D code.

Most definitions are probably generic enough to be part of Phobos.

See_Also: http://forum.dlang.org/post/tppptevxiygafzpicmgz@forum.dlang.org

It includes various kinds of

- Integer sorting algorithms, including radix sort: `integer_sorting.d`
- Clever printing of groups of arrays/slices: `show.d`
- Boyer-Moore-Hoorspool search: `horspool.d`
- Symbol regex (structured regular expressions similar to elisps rx): `symbolic.d`
- A fixed-size (statically stack-allocated) variant of `bitarray.d` in `static_bitarray.d`
- An n-gram implementation (many nested for loops): `ngram.d`
- A wrapper for bounded types: `bound.d`
- Computer Science units: `csunits.d`
- Enhanced `NotNull`: `notnull.d`
- A structured wrapper for message digests: `digest_ex.d`
- Various extension to Phobos (often ending with _ex.d)
