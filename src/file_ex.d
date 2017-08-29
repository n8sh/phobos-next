module file_ex;

/** Read file $(D path) into raw array with one extra terminating zero byte.

    This extra terminating zero (null) byte at the end is typically used as a
    sentinel value to speed up textual parsers.

    TODO add or merge to Phobos?

    See also: https://en.wikipedia.org/wiki/Sentinel_value
    See also: http://forum.dlang.org/post/pdzxpkusvifelumkrtdb@forum.dlang.org
*/
immutable(void)[] rawReadNullTerminated(string path)
    @trusted
{
    import std.stdio : File;
    import std.exception : assumeUnique;

    auto file = File(path, `rb`);

    import std.array : uninitializedArray;
    ubyte[] data = uninitializedArray!(ubyte[])(file.size + 1); // one extra for null terminator
    file.rawRead(data);
    data[file.size] = 0;     // null terminator for sentinel

    return assumeUnique(data);                // TODO can we cast this to immutable?
}

@safe unittest
{
    import std.file;
    rawReadNullTerminated(make);
}
