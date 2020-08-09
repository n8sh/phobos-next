/** File I/O of Compressed Files.
 *
 * See_Also: https://forum.dlang.org/post/jykarqycnrecajveqpos@forum.dlang.org
 */
module nxt.zio;

import std.range.primitives : isInputRange;

@safe:

struct GzipFileInputRange
{
    import std.stdio : File;
    import std.traits : ReturnType;

    enum chunkSize = 0x4000;    // TODO: find optimal value via benchmark

    enum defaultExtension = `.gz`;

    this(in char[] path) @trusted
    {
        _f = File(path, `r`);
        _chunkRange = _f.byChunk(chunkSize);
        _uncompress = new UnCompress;
        loadNextChunk();
    }

    void loadNextChunk() @trusted
    {
        if (!_chunkRange.empty)
        {
            _uncompressedBuf = cast(ubyte[])_uncompress.uncompress(_chunkRange.front);
            _chunkRange.popFront();
        }
        else
        {
            if (!_exhausted)
            {
                _uncompressedBuf = cast(ubyte[])_uncompress.flush();
                _exhausted = true;
            }
            else
            {
                _uncompressedBuf.length = 0;
            }
        }
        _bufIx = 0;
    }

    void popFront()
    {
        _bufIx += 1;
        if (_bufIx >= _uncompressedBuf.length)
        {
            loadNextChunk();
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
    import std.zlib : UnCompress;
    UnCompress _uncompress;
    File _f;
    ReturnType!(_f.byChunk) _chunkRange;
    bool _exhausted;            ///< True if exhausted.
    ubyte[] _uncompressedBuf;   ///< Uncompressed buffer.
    size_t _bufIx;              ///< Current byte index into `_uncompressedBuf`.
}

/** Is `true` iff `R` is a block input range.
    TODO: Move to std.range
 */
private template isBlockInputRange(R)
{
    import std.range.primitives : isInputRange;
    enum isBlockInputRange = (isInputRange!R &&
                              __traits(hasMember, R, `bufferFrontChunk`) && // TODO: ask dlang for better naming
                              __traits(hasMember, R, `loadNextChunk`));     // TODO: ask dlang for better naming
}

/** Decompress `BlockInputRange` linewise.
 */
class DecompressByLine(BlockInputRange)
{
    private alias E = char;

    /** If `range` is of type `isBlockInputRange` decoding compressed files will
     * be much faster.
     */
    this(in const(char)[] range,
         E separator = '\n',
         in size_t initialCapacity = 80)
    {
        this._range = typeof(_range)(range);
        this._separator = separator;
        static if (__traits(hasMember, typeof(_lbuf), `withCapacity`))
        {
            this._lbuf = typeof(_lbuf).withCapacity(initialCapacity);
        }
        popFront();
    }

    void popFront() @trusted
    {
        _lbuf.shrinkTo(0);

        static if (isBlockInputRange!(typeof(_range)))
        {
            // TODO: functionize
            while (!_range.empty)
            {
                ubyte[] currentFronts = _range.bufferFrontChunk;
                // `_range` is mutable so sentinel-based search can kick

                enum useCountUntil = false;
                static if (useCountUntil)
                {
                    import std.algorithm.searching : countUntil;
                    // TODO
                }
                else
                {
                    import std.algorithm.searching : find;
                    const hit = currentFronts.find(_separator); // or use `indexOf`
                }

                if (hit.length)
                {
                    const lineLength = hit.ptr - currentFronts.ptr;
                    _lbuf.put(currentFronts[0 .. lineLength]); // add everything up to separator
                    _range._bufIx += lineLength + _separator.sizeof; // advancement + separator
                    if (_range.empty)
                    {
                        _range.loadNextChunk();
                    }
                    break;      // done
                }
                else            // no separator yet
                {
                    _lbuf.put(currentFronts); // so just add everything
                    _range.loadNextChunk();
                }
            }
        }
        else
        {
            // TODO: sentinel-based search for `_separator` in `_range`
            while (!_range.empty &&
                   _range.front != _separator)
            {
                _lbuf.put(_range.front);
                _range.popFront();
            }

            if (!_range.empty &&
                _range.front == _separator)
            {
                _range.popFront();  // pop separator
            }
        }
    }

    pragma(inline):
    @safe pure nothrow @nogc:

    @property bool empty() const
    {
        return _lbuf.data.length == 0;
    }

    const(E)[] front() const return scope
    {
        return _lbuf.data;
    }

private:
    BlockInputRange _range;

    import std.array : Appender;
    Appender!(E[]) _lbuf;       // line buffer

    // NOTE this is slower for ldc:
    // import nxt.dynamic_array : Array;
    // Array!E _lbuf;

    E _separator;
}

class GzipOut
{
    import std.zlib: Compress, HeaderFormat;
    import std.stdio: File;

    this(File file) @trusted
    {
        _f = file;
        _compress = new Compress(HeaderFormat.gzip);
    }

    void compress(const string s) @trusted
    {
        auto compressed = _compress.compress(s);
        _f.rawWrite(compressed);
    }

    void finish() @trusted
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
    import std.file : FileException;

    /* Zlib docs:
       CHUNK is simply the buffer size for feeding data to and pulling data from
       the zlib routines. Larger buffer sizes would be more efficient,
       especially for inflate(). If the memory is available, buffers sizes on
       the order of 128K or 256K bytes should be used.
    */
    enum chunkSize = 128 * 1024; // 128K

    enum defaultExtension = `.gz`;

    @safe:

    this(in char[] path) @trusted
    {
        import std.string : toStringz; // TODO: avoid GC allocation by looking at how gmp-d z.d solves it
        _f = gzopen(path.toStringz, `rb`);
        if (!_f)
        {
            throw new FileException(`Couldn't open file ` ~ path.idup);
        }
        _buf = new ubyte[chunkSize];
        loadNextChunk();
    }

    ~this() @trusted @nogc
    {
        const int ret = gzclose(_f);
        if (ret < 0)
        {
            assert(`Couldn't close file`); // TODO: replace with non-GC-allocated exception
        }
    }

    @disable this(this);

    void loadNextChunk() @trusted
    {
        int count = gzread(_f, _buf.ptr, chunkSize);
        if (count == -1)
        {
            throw new Exception(`Error decoding file`);
        }
        _bufIx = 0;
        _bufReadLength = count;
    }

    void popFront()
    {
        assert(!empty);
        _bufIx += 1;
        if (_bufIx >= _bufReadLength)
        {
            loadNextChunk();
            _bufIx = 0;         // restart counter
        }
    }

pragma(inline, true):
pure nothrow @nogc:

    @property ubyte front() const @trusted
    {
        assert(!empty);
        return _buf.ptr[_bufIx];
    }

    @property bool empty() const
    {
        return _bufIx == _bufReadLength;
    }

    /** Get current bufferFrontChunk.
        TODO: need better name for this
     */
    inout(ubyte)[] bufferFrontChunk() inout @trusted
    {
        assert(!empty);
        return _buf.ptr[_bufIx .. _bufReadLength];
    }

private:
    import etc.c.zlib : gzFile, gzopen, gzclose, gzread;

    gzFile _f;

    ubyte[] _buf;               // block read buffer

    // number of bytes in `_buf` recently read by `gzread`, normally equal to `_buf.length` except after last read where is it's normally less than `_buf.length`
    size_t _bufReadLength;

    size_t _bufIx;              // current stream read index in `_buf`

    // TODO: make this work:
    // extern (C) nothrow @nogc:
    // pragma(mangle, `gzopen`) gzFile gzopen(const(char)* path, const(char)* mode);
    // pragma(mangle, `gzclose`) int gzclose(gzFile file);
    // pragma(mangle, `gzread`) int gzread(gzFile file, void* buf, uint len);
}

struct Bz2libFileInputRange
{
    import std.file : FileException;

    enum chunkSize = 128 * 1024; // 128K. TODO: find optimal value via benchmark
    enum defaultExtension = `.bz2`;
    enum useGC = false;         // TODO: generalize to allocator parameter

@safe:

    this(in char[] path) @trusted
    {
        import std.string : toStringz; // TODO: avoid GC allocation by looking at how gmp-d z.d solves it
        _f = BZ2_bzopen(path.toStringz, `rb`);
        if (!_f)
        {
            throw new FileException(`Couldn't open file ` ~ path.idup);
        }

        static if (useGC)
        {
            _buf = new ubyte[chunkSize];
        }
        else
        {
            import core.memory : pureMalloc;
            _buf = (cast(ubyte*)pureMalloc(chunkSize))[0 .. chunkSize];
        }

        loadNextChunk();
    }

    ~this() @trusted @nogc
    {
        BZ2_bzclose(_f);       // TODO: error handling?

        static if (!useGC)
        {
            import core.memory : pureFree;
            pureFree(_buf.ptr);
        }
    }

    @disable this(this);

    void loadNextChunk() @trusted
    {
        int count = BZ2_bzread(_f, _buf.ptr, chunkSize);
        if (count == -1)
        {
            throw new Exception(`Error decoding file`);
        }
        _bufIx = 0;
        _bufReadLength = count;
    }

    void popFront()
    {
        assert(!empty);
        _bufIx += 1;
        if (_bufIx >= _bufReadLength)
        {
            loadNextChunk();
            _bufIx = 0;         // restart counter
        }
    }

    pragma(inline, true):
    pure nothrow @nogc:

    @property ubyte front() const @trusted
    {
        assert(!empty);
        return _buf.ptr[_bufIx];
    }

    @property bool empty() const
    {
        return _bufIx == _bufReadLength;
    }

    /** Get current bufferFrontChunk.
        TODO: need better name for this
     */
    inout(ubyte)[] bufferFrontChunk() inout @trusted
    {
        assert(!empty);
        return _buf.ptr[_bufIx .. _bufReadLength];
    }

private:
    import nxt.bzlib : BZFILE, BZ2_bzopen, BZ2_bzread, BZ2_bzwrite, BZ2_bzclose;
    pragma(lib, `bz2`);             // Ubuntu: sudo apt-get install libbz2-dev

    BZFILE* _f;

    ubyte[] _buf;               // block read buffer

    // number of bytes in `_buf` recently read by `gzread`, normally equal to `_buf.length` except after last read where is it's normally less than `_buf.length`
    size_t _bufReadLength;

    size_t _bufIx;              // current stream read index in `_buf`
}

void testInputRange(FileInputRange)() @safe
if (isInputRange!FileInputRange)
{
    import std.stdio : File;

    enum path = `test` ~ FileInputRange.defaultExtension;

    const wholeSource = "abc\ndef\nghi"; // contents of source

    foreach (const n; wholeSource.length .. wholeSource.length) // TODO: from 0
    {
        const source = wholeSource[0 .. n]; // slice from the beginning

        File file = File(path, `w`); // TODO: `scope`
        auto of = new GzipOut(file); // TODO: `scope`
        of.compress(source);
        of.finish();

        size_t ix = 0;
        foreach (e; FileInputRange(path))
        {
            assert(cast(char)e == source[ix]);
            ++ix;
        }

        import std.algorithm.searching : count;
        import std.algorithm.iteration : splitter;
        alias R = DecompressByLine!ZlibFileInputRange;

        assert(new R(path).count == source.splitter('\n').count);
    }
}

@safe unittest
{
    testInputRange!(GzipFileInputRange);
    testInputRange!(ZlibFileInputRange);
    testInputRange!(Bz2libFileInputRange);
}

/** Read Age of Aqcuisitions.
 */
static private void testReadAgeofAqcuisitions(const string rootDirPath = `~/Work/knet/knowledge/en/age-of-aqcuisition`) @safe
{
    import std.path: expandTilde;
    import nxt.zio : DecompressByLine, GzipFileInputRange;
    import std.path : buildNormalizedPath;

    {
        const path = buildNormalizedPath(rootDirPath.expandTilde,
                                         `AoA_51715_words.csv.gz`);
        size_t count = 0;
        foreach (line; new DecompressByLine!GzipFileInputRange(path))
        {
            count += 1;
        }
        assert(count == 51716);
    }

    {
        const path = buildNormalizedPath(rootDirPath.expandTilde,
                                         `AoA_51715_words.csv.gz`);
        size_t count = 0;
        foreach (line; new DecompressByLine!ZlibFileInputRange(path))
        {
            count += 1;
        }
        assert(count == 51716);
    }

    {
        const path = buildNormalizedPath(rootDirPath.expandTilde,
                                         `AoA_51715_words_copy.csv.bz2`);
        size_t count = 0;
        foreach (line; new DecompressByLine!Bz2libFileInputRange(path))
        {
            count += 1;
        }
        assert(count == 51716);
    }
}

/** Read Concept 5 assertions.
 */
static private void testReadConcept5Assertions(const string path = `/home/per/Knowledge/ConceptNet5/latest/conceptnet-assertions-5.6.0.csv.gz`) @safe
{
    alias R = ZlibFileInputRange;

    import std.stdio: writeln;
    import std.range: take;
    import std.algorithm.searching: count;

    const lineBlockCount = 100_000;
    size_t lineNr = 0;
    foreach (const line; new DecompressByLine!R(path))
    {
        if (lineNr % lineBlockCount == 0)
        {
            writeln(`Line `, lineNr, ` read containing:`, line);
        }
        lineNr += 1;
    }

    const lineCount = 5;
    foreach (const line; new DecompressByLine!R(path).take(lineCount))
    {
        writeln(line);
    }
}

/// benchmark DBpedia parsing
static private void benchmarkDbpediaParsing(const string rootPath = `/home/per/Knowledge/DBpedia/latest`) @system
{
    alias R = Bz2libFileInputRange;

    import nxt.array_algorithm : startsWith, endsWith;
    import std.algorithm : filter;
    import std.file : dirEntries, SpanMode;
    import std.path : baseName;
    import std.stdio : write, writeln, stdout;
    import std.datetime : MonoTime;

    foreach (const path; dirEntries(rootPath, SpanMode.depth).filter!(file => (file.name.baseName.startsWith(`instance_types`) &&
                                                                               file.name.endsWith(`.ttl.bz2`))))
    {
        write(`Checking `, path, ` ... `); stdout.flush();

        immutable before = MonoTime.currTime();

        size_t lineCounter = 0;
        foreach (const line; new DecompressByLine!R(path))
        {
            lineCounter += 1;
        }

        immutable after = MonoTime.currTime();

        showStat(path, before, after, lineCounter);
    }
}

/// Show statistics.
static private void showStat(T)(in const(char[]) tag,
                                in T before,
                                in T after,
                                in size_t lineCount)
{
    import std.stdio : writefln;
    writefln(`%s: %3.1f msecs (%3.1f usecs/line)`,
             tag,
             cast(double)(after - before).total!`msecs`,
             cast(double)(after - before).total!`usecs` / lineCount);
}
