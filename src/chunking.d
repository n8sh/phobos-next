module chunking;

@system unittest
{
    import std.algorithm;
    import std.algorithm.comparison : equal;
    auto r = [1, 2, 3, 4, 5, 6, 7, 8, 9].chunkBy!((x, y) => ((x*y) % 3) == 0);
    assert(r.equal!equal([
        [1],
        [2, 3, 4],
        [5, 6, 7],
        [8, 9]
    ]));
}
