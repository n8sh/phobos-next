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
    // import nxt.dbgio;
    // dbg("Size:", file.size);
    // ubyte[] data = new ubyte[](file.size + 1);
    ubyte[] data = uninitializedArray!(ubyte[])(file.size + 1); // one extra for terminator
    file.rawRead(data);
    data[file.size] = 0;     // zero terminator for sentinel

    import std.exception : assumeUnique;
    return assumeUnique(data);
}
