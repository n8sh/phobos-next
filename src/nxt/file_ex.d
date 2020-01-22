module nxt.file_ex;

/** Read file $(D path) into raw array with one extra terminating zero byte.
 *
 * This extra terminating zero (`null`) byte at the end is typically used as a
 * sentinel value to speed up textual parsers.
 *
 * TODO add or merge to Phobos?
 *
 * See_Also: https://en.wikipedia.org/wiki/Sentinel_value
 * See_Also: http://forum.dlang.org/post/pdzxpkusvifelumkrtdb@forum.dlang.org
 */
immutable(void)[] rawReadPath(string path) @trusted
{
    import std.array : uninitializedArray;

    import std.stdio : File;
    auto file = File(path, `rb`);

    alias Data = ubyte[];

    const bool appendTerminatingNull;
    const totalSize = appendTerminatingNull ? file.size + 1 : file.size;

    Data data = uninitializedArray!(Data)(totalSize); // one extra for terminator

    file.rawRead(data);

    if (totalSize)
    {
        data[file.size] = 0;     // zero terminator for sentinel
    }

    import std.exception : assumeUnique;
    return assumeUnique(data);
}
