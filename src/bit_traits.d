module bit_traits;

/** Get number of bits needed to represent the range (0 .. `length`-1). */
private template bitsNeeded(size_t length)
{
    static      if (length <= 2)   { enum bitsNeeded = 1; }
    else static if (length <= 4)   { enum bitsNeeded = 2; }
    else static if (length <= 8)   { enum bitsNeeded = 3; }
    else static if (length <= 16)  { enum bitsNeeded = 4; }
    else static if (length <= 32)  { enum bitsNeeded = 5; }
    else static if (length <= 64)  { enum bitsNeeded = 6; }
    else static if (length <= 128) { enum bitsNeeded = 7; }
    else static if (length <= 256) { enum bitsNeeded = 8; }
    else static if (length <= 512) { enum bitsNeeded = 9; }
    else                           { static assert(0, `Too large length`); }
}

/** Number of bits required to store a packed instance of $(D T).
    See also: http://forum.dlang.org/thread/okonqhnxzqlqtxijxsfg@forum.dlang.org

    TODO Extend to continuous version; use std.numeric.sumOfLog2s. Ask on
    StackExchange Computer Science for the correct terminology.

    See: http://dlang.org/phobos/std_numeric.html#.sumOfLog2s

    TODO merge with `UsageOf`
   */
template packedBitSizeOf(T)
{
    static if (is(T == enum))
    {
        static assert(T.min != T.max, "enum T must have at least two enumerators");
        import core.bitop : bsr;
        enum range = T.max - T.min; // TODO use uniqueEnumMembers.length instead?
        enum packedBitSizeOf = range.bsr + 1;
    }
    // TODO
    // else static if (isAggregate!T)
    // {
    //     foreach (E; T.tupleof)
    //     {
    //         ....;
    //     }
    // }
    else
    {
        enum packedBitSizeOf = 8*T.sizeof;
    }
}

@safe pure nothrow @nogc unittest
{
    static assert(packedBitSizeOf!ubyte == 8);
    static assert(!__traits(compiles, { enum E1 { x } static assert(packedBitSizeOf!E1 == 1);}));
    enum E2 { x, y }
    static assert(packedBitSizeOf!E2 == 1);
    enum E3 { x, y, z }
    static assert(packedBitSizeOf!E3 == 2);
    enum E4 { x, y, z, w }
    static assert(packedBitSizeOf!E4 == 2);
    enum E5 { a, b, c, d, e }
    static assert(packedBitSizeOf!E5 == 3);
    enum E6 { a, b, c, d, e, f }
    static assert(packedBitSizeOf!E6 == 3);
    enum E7 { a, b, c, d, e, f, g }
    static assert(packedBitSizeOf!E7 == 3);
    enum E8 { a, b, c, d, e, f, g, h }
    static assert(packedBitSizeOf!E8 == 3);
    enum E9 { a, b, c, d, e, f, g, h, i }
    static assert(packedBitSizeOf!E9 == 4);
}
