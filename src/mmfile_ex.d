module mmfile_ex;

/** Read-Only Lines of Contents of file $(D path).
    TODO Use new std.string.splitLines() (via Phobos pull 2982).
   */
auto mmFileLinesRO(ElementType = char)(string path)
    if
    (ElementType.sizeof == 1)
{
    version(linux)
    {
        import core.sys.posix.sys.shm: __getpagesize;
        const pageSize = __getpagesize();
    }
    else
    {
        const pageSize = 4096;
    }
    import std.mmfile: MmFile;
    import std.path: expandTilde, buildNormalizedPath;
    auto mmf = new MmFile(path.expandTilde.buildNormalizedPath,
                          MmFile.Mode.read, 0, null, pageSize);
    import byline : byLine, Newline;
    return (cast(ElementType[])mmf[]).byLine!(Newline.native);
}
