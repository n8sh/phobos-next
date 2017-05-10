/** File I/O of Compressed Files.
*/
module zio;

import bylinefast;
import bzlib;

struct GzipFileInputRange
{
    import std.stdio : File;
    import std.traits : ReturnType;

    enum CHUNKSIZE = 0x4000;

    this(in const(char)[] path)
    {
        _f = File(path, "r");
        _chunkRange = _f.byChunk(CHUNKSIZE);
        _uncompress = new UnCompress;
        load();
    }

    void load()
    {
        if (!_chunkRange.empty)
        {
            _uncompressedBuffer = cast(ubyte[])_uncompress.uncompress(_chunkRange.front);
            _chunkRange.popFront();
            _bufferIndex = 0;
        }
        else
        {
            if (!_exhausted)
            {
                _uncompressedBuffer = cast(ubyte[])_uncompress.flush();
                _exhausted = true;
                _bufferIndex = 0;
            }
            else
            {
                _uncompressedBuffer.length = 0;
            }
        }
    }

    void popFront()
    {
        _bufferIndex += 1;
        if (_bufferIndex >= _uncompressedBuffer.length)
        {
            load();
            _bufferIndex = 0;
        }
    }

    pragma(inline, true):
    @safe pure nothrow @nogc:

    @property ubyte front() const
    {
        return _uncompressedBuffer[_bufferIndex];
    }

    @property bool empty() const
    {
        return _uncompressedBuffer.length == 0;
    }

private:
    import std.zlib: UnCompress;
    UnCompress _uncompress;
    File _f;
    ReturnType!(_f.byChunk) _chunkRange;
    bool _exhausted;
    ubyte[] _uncompressedBuffer;
    size_t _bufferIndex;
}

class GzipByLine
{
    this(in const(char)[] path,
         char separator = '\n')
    {
        this._range = GzipFileInputRange(path);
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
    @safe /*@nogc*/:

    this(in const(char)[] path) @trusted
    {
        import std.string : toStringz;
        _f = gzopen(path.toStringz, `rb`);
        if (!_f)
        {
            throw new Exception("Couldn't open file " ~ path.idup);
        }
    }

    ~this() @trusted
    {
        gzclose(_f);
    }

    @disable this(this);

    pure nothrow @nogc:

private:
    import etc.c.zlib;
    gzFile _f;
}

unittest
{
    enum path = "test.gz";

    auto of = new GzipOut(path);
    of.compress("bla\nbla\nbla");
    of.finish();

    import std.algorithm.searching : count;
    assert(new GzipByLine(path).count == 3);

    auto zfi = ZlibFileInputRange(path);
}

// version(none)
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
