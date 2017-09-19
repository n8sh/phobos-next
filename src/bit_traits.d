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
    else                           { static assert(false, `Too large length`); }
}
