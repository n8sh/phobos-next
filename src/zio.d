#!/usr/bin/env rdmd-dev-module

/** File I/O of Compressed Files.
*/
module zio;

import bylinefast;
import bzlib;

class GzipFileInputRange
{
    import std.stdio: File;
    import std.traits: ReturnType;

    enum CHUNKSIZE = 0x4000;

    this(string filename)
    {
        _f = File(filename, "r");
        _chunkRange = _f.byChunk(CHUNKSIZE);
        _uncompress = new UnCompress;
        load;
    }

    void load()
    {
        if (!_chunkRange.empty)
        {
            _uncompressedBuffer = cast(char[])_uncompress.uncompress(_chunkRange.front.dup);
            _chunkRange.popFront();
            _bufferIndex = 0;
        }
        else
        {
            if (!_exhausted)
            {
                _uncompressedBuffer = cast(char[])_uncompress.flush;
                _exhausted = true;
                _bufferIndex = 0;
            }
            else
            {
                _uncompressedBuffer.length = 0;
            }
        }
    }

    @property char front() @safe @nogc pure nothrow
    {
        return _uncompressedBuffer[_bufferIndex];
    }

    void popFront()
    {
        _bufferIndex += 1;
        if (_bufferIndex >= _uncompressedBuffer.length)
        {
            load;
            _bufferIndex = 0;
        }
    }

    @property bool empty() @safe @nogc pure nothrow
    {
        return _uncompressedBuffer.length == 0;
    }

private:
    import std.zlib: UnCompress;

    UnCompress _uncompress;
    File _f;
    ReturnType!(_f.byChunk) _chunkRange;
    bool _exhausted;
    char[] _uncompressedBuffer;
    size_t _bufferIndex;
}

class GzipByLine
{
    this(string filename)
    {
        this._range = new GzipFileInputRange(filename);
        popFront;
    }

    @property bool empty() @safe @nogc pure nothrow
    {
        return _buf.length == 0;
    }

    void popFront()
    {
        _buf.length = 0;
        while (!_range.empty && _range.front != '\n')
        {
            _buf ~= _range.front;
            _range.popFront();
        }
        _range.popFront();
    }

    auto front() @safe pure nothrow // TODO should this mutable or not?
    {
        return _buf;
    }

    string ifront() @safe pure nothrow
    {
        return _buf.idup;
    }

private:
    GzipFileInputRange _range;
    char[] _buf;
}

class GzipOut
{
    import std.zlib: Compress, HeaderFormat;
    import std.stdio: File;

    this(string filename)
    {
        _f = File(filename, "w");
        _compress = new Compress(HeaderFormat.gzip);
    }

    void compress(const string s)
    {
        auto compressed = _compress.compress(s); // TODO dup needed?
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

unittest
{
    enum fileName = "test.gz";

    auto of = new GzipOut(fileName);
    of.compress("bla\nbla\nbla");
    of.finish;

    import std.algorithm.searching: count;

    assert(new GzipByLine(fileName).count == 3);
}

version(none) unittest
{
    enum fileName = "/home/per/Knowledge/DBpedia/disambiguations_en2.nt.gz";

    import std.stdio: writeln;
    import std.range: take;
    import std.algorithm.searching: count;

    writeln(new GzipByLine(fileName).count);

    foreach (immutable line; new GzipByLine(fileName).take(5))
    {
        writeln(line);
    }
}
