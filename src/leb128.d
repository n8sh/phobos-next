/** LEB128 (Little Endian Base 128).
   See also: https://en.wikipedia.org/wiki/LEB128
*/
module leb128;

import std.range : isOutputRange;
import std.traits : isUnsigned, isSigned;

/// Encode a SLEB128 value to `os`.
void encodeSLEB128(Output)(ref Output os, long value)
    if (isOutputRange!(Output, ubyte))
{
    bool more;
    do
    {
        ubyte byte_ = value & 0x7f;
        // assumes that this signed shift is an arithmetic right shift
        value >>= 7;
        more = !(((value == 0 ) && ((byte_ & 0x40) == 0)) ||
                  ((value == -1) && ((byte_ & 0x40) != 0)));
        if (more)
            byte_ |= 0x80; // mark this byte to show that more bytes will follow
        os.put(byte_);
    }
    while (more);
}

version(unittest)
{
    import dbgio : dln;
    import std.algorithm.comparison : equal;
    import std.array : Appender;
}

@safe pure nothrow unittest
{
    foreach (immutable i; 0 .. 64)
    {
        Appender!(ubyte[]) os;
        os.encodeSLEB128(i);
        assert(os.data.equal([i]));
    }
    foreach (immutable i; 64 .. 128)
    {
        Appender!(ubyte[]) os;
        os.encodeSLEB128(i);
        assert(os.data.equal([128 + i, 0]));
    }
}

/// Encode a ULEB128 value to `os`.
void encodeULEB128(Output)(ref Output os, ulong value)
    if (isOutputRange!(Output, ubyte))
{
    do
    {
        ubyte byte_ = value & 0x7f;
        value >>= 7;
        if (value != 0)
            byte_ |= 0x80; // mark this byte to show that more bytes will follow
        os.put(char(byte_));
    }
    while (value != 0);
}

@safe pure nothrow unittest
{
    foreach (immutable i; 0 .. 128)
    {
        Appender!(ubyte[]) os;
        os.encodeULEB128(i);
        assert(os.data.equal([i]));
    }
    foreach (immutable i; 128 .. 256)
    {
        Appender!(ubyte[]) os;
        os.encodeULEB128(i);
        assert(os.data.equal([i, 1]));
    }
}

/** Encode a ULEB128 value to a buffer.
    Returns: length in bytes of the encoded value.
*/
uint encodeULEB128(ulong value, ubyte *p,
                   uint padding = 0)
{
    ubyte *orig_p = p;
    do
    {
        ubyte byte_ = value & 0x7f;
        value >>= 7;
        if (value != 0 || padding != 0)
            byte_ |= 0x80; // mark this byte to show that more bytes will follow
        *p++ = byte_;
    }
    while (value != 0);

    // pad with 0x80 and emit a null byte at the end
    if (padding != 0)
    {
        for (; padding != 1; --padding)
            *p++ = '\x80';
        *p++ = '\x00';
    }
    return cast(uint)(p - orig_p);
}

@safe pure nothrow @nogc unittest
{

}

/// Decode a ULEB128 value.
ulong decodeULEB128(ubyte *p, uint *n = null)
{
    const ubyte *orig_p = p;
    ulong value = 0;
    uint shift = 0;
    do
    {
        value += ulong(*p & 0x7f) << shift;
        shift += 7;
    }
    while (*p++ >= 128);
    if (n)
        *n = cast(uint)(p - orig_p);
    return value;
}

/// Decode a SLEB128 value.
long decodeSLEB128(ubyte *p, uint *n = null)
{
    const ubyte *orig_p = p;
    long value = 0;
    uint shift = 0;
    ubyte byte_;
    do
    {
        byte_ = *p++;
        value |= ((byte_ & 0x7f) << shift);
        shift += 7;
    } while (byte_ >= 128);
    // sign extend negative numbers
    if (byte_ & 0x40)
        value |= (cast(ulong)-1) << shift; // value |= (-1ULL) << shift;
    if (n)
        *n = cast(uint)(p - orig_p);
    return value;
}
