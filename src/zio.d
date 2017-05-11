/** File I/O of Compressed Files.
*/
module zio;

import bylinefast;
import bzlib;

struct GzipFileInputRange
{
    import std.stdio : File;
    import std.traits : ReturnType;

    enum chunkSize = 0x4000;

    this(in const(char)[] path)
    {
        _f = File(path, "r");
        _chunkRange = _f.byChunk(chunkSize);
        _uncompress = new UnCompress;
        load();
    }

    void load()
    {
        if (!_chunkRange.empty)
        {
            _uncompressedBuf = cast(ubyte[])_uncompress.uncompress(_chunkRange.front);
            _chunkRange.popFront();
            _bufIx = 0;
        }
        else
        {
            if (!_exhausted)
            {
                _uncompressedBuf = cast(ubyte[])_uncompress.flush();
                _exhausted = true;
                _bufIx = 0;
            }
            else
            {
                _uncompressedBuf.length = 0;
            }
        }
    }

    void popFront()
    {
        _bufIx += 1;
        if (_bufIx >= _uncompressedBuf.length)
        {
            load();
            _bufIx = 0;
        }
    }

    pragma(inline, true):
    @safe pure nothrow @nogc:

    @property ubyte front() const
    {
        return _uncompressedBuf[_bufIx];
    }

    @property bool empty() const
    {
        return _uncompressedBuf.length == 0;
    }

private:
    import std.zlib: UnCompress;
    UnCompress _uncompress;
    File _f;
    ReturnType!(_f.byChunk) _chunkRange;
    bool _exhausted;
    ubyte[] _uncompressedBuf;
    size_t _bufIx;
}

class GzipByLine
{
    this(in const(char)[] path,
         char separator = '\n')
    {
        this._range = typeof(_range)(path);
        this._separator = separator;
        popFront();
    }

    void popFront()
    {
        _buf.shrinkTo(0);
        while (!_range.empty &&
               _range.front != _separator)
        {
            _buf.put(_range.front);
            _range.popFront();
        }
        _range.popFront();
    }

    pragma(inline, true):
    @safe pure nothrow @nogc:

    @property bool empty()
    {
        return _buf.data.length == 0;
    }

    const(char)[] front() const return scope
    {
        return _buf.data;
    }

private:
    GzipFileInputRange _range;
    import std.array : Appender;
    Appender!(char[]) _buf;
    char _separator;
}

class GzipOut
{
    import std.zlib: Compress, HeaderFormat;
    import std.stdio: File;

    this(string path)
    {
        _f = File(path, "w");
        _compress = new Compress(HeaderFormat.gzip);
    }

    void compress(const string s)
    {
        auto compressed = _compress.compress(s);
        _f.rawWrite(compressed);
    }

    void finish()
    {
        auto compressed = _compress.flush;
        _f.rawWrite(compressed);
        _f.close;
    }

private:
    Compress _compress;
    File _f;
}

struct ZlibFileInputRange
{
    /* Zlib docs:
       CHUNK is simply the buffer size for feeding data to and pulling data from
       the zlib routines. Larger buffer sizes would be more efficient,
       especially for inflate(). If the memory is available, buffers sizes on
       the order of 128K or 256K bytes should be used.
    */
    enum chunkSize = 128 * 1024; // 128K

    @safe:

    this(in const(char)[] path) @trusted
    {
        import std.string : toStringz; // TODO avoid GC allocation by looking at how gmp-d z.d solves it
        _f = gzopen(path.toStringz, `rb`);
        if (!_f)
        {
            throw new Exception("Couldn't open file " ~ path.idup);
        }
        _buf = new ubyte[chunkSize];
        load();
    }

    ~this() @trusted nothrow
    {
        gzclose(_f);
    }

    @disable this(this);

    void load() @trusted
    {
        int count = gzread(_f, _buf.ptr, chunkSize);
        if (count == -1)
        {
            throw new Exception("Error decoding file");
        }
        _bufLength = count;
    }

    void popFront()
    {
        assert(!empty);
        _bufIx += 1;
        if (_bufIx >= _bufLength)
        {
            load();
            _bufIx = 0;         // restart counter
        }
    }

    pragma(inline, true):
    pure nothrow @nogc:

    @property ubyte front() const
    {
        assert(!empty);
        return _buf[_bufIx]; // TODO use .ptr[]
    }

    @property bool empty() const
    {
        return _bufIx == _bufLength;
    }

private:
    import etc.c.zlib;

    gzFile _f;

    import std.array : Appender;
    ubyte[] _buf;
    size_t _bufLength;
    size_t _bufIx;

    // TODO make this work:
    // extern (C) nothrow @nogc:
    // pragma(mangle, "gzopen") gzFile gzopen(const(char)* path, const(char)* mode);
    // pragma(mangle, "gzclose") int gzclose(gzFile file);
    // pragma(mangle, "gzread") int gzread(gzFile file, void* buf, uint len);
}

unittest
{
    enum path = "test.gz";
    const source = "bla\nbla\nbla";

    auto of = new GzipOut(path);
    of.compress(source);
    of.finish();

    import std.algorithm.searching : count;
    assert(new GzipByLine(path).count == 3);

    auto zfi = ZlibFileInputRange(path);

    foreach (e; ZlibFileInputRange(path))
    {
        write(cast(char)e);
    }
}

version(none)
unittest
{
    enum path = "/home/per/Knowledge/ConceptNet5/5.5/conceptnet-assertions-5.5.0.csv.gz";

    import std.stdio: writeln;
    import std.range: take;
    import std.algorithm.searching: count;

    const lineBlockCount = 100_000;
    size_t lineNr = 0;
    foreach (const line; new GzipByLine(path))
    {
        if (lineNr % lineBlockCount == 0)
        {
            writeln(`Line `, lineNr, ` read containing:`, line);
        }
        lineNr += 1;
    }

    const lineCount = 5;
    foreach (const line; new GzipByLine(path).take(lineCount))
    {
        writeln(line);
    }
}

version(unittest)
{
    import std.stdio : write, writeln;
}
