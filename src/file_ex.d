module file_ex;

/** Read file $(D path) into raw array with one extra terminating zero (null)
    byte.

    TODO add or merge to Phobos?

    See also: https://en.wikipedia.org/wiki/Sentinel_value
    See also: http://forum.dlang.org/post/pdzxpkusvifelumkrtdb@forum.dlang.org
*/
void[] rawReadNullTerminated(string path)
    @safe
{
    import std.stdio : File;
    auto file = File(path, `rb`);

    import std.array : uninitializedArray;
    ubyte[] data = uninitializedArray!(ubyte[])(file.size + 1); // one extra for null terminator
    file.rawRead(data);
    data[file.size] = 0;     // null terminator for sentinel

    return data;                // TODO can we cast this to immutable?
}
