module nxt.file_ex;

/** Read file $(D path) into raw array with one extra terminating zero byte.
 *
 * This extra terminating zero (null) byte at the end is typically used as a
 * sentinel value to speed up textual parsers.
 *
 * TODO add or merge to Phobos?
 *
 * See_Also: https://en.wikipedia.org/wiki/Sentinel_value
 * See_Also: http://forum.dlang.org/post/pdzxpkusvifelumkrtdb@forum.dlang.org
 */
immutable(void)[] rawReadNullTerminated(string path) @trusted
{
    import std.stdio : File;
    auto file = File(path, `rb`);

    import std.array : uninitializedArray;
    import nxt.dbgio;

    alias Data = ubyte[];

    const extraTest = false;
    if (extraTest)
    {
        size_t n = 1;
        while (n < 1_000_000_000)
        {
            dbg("Allocating ", n, " bytes ...");
            Data _ = new Data(n);
            n *= 2;
        }
    }

    Data data = uninitializedArray!(Data)(file.size + 1); // one extra for terminator
    dbg();
    file.rawRead(data);
    data[file.size] = 0;     // zero terminator for sentinel

    import std.exception : assumeUnique;
    return assumeUnique(data);
}
