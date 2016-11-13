/** LEB128 (Little Endian Base 128).
   See also: https://en.wikipedia.org/wiki/LEB128
*/
module leb128;

import std.traits : isUnsigned, isSigned;

auto encode(T)(T x)
    if (isUnsigned!T)
{
//     do {
//   byte = low order 7 bits of value;
//   value >>= 7;
//   if (value != 0) /* more bytes to come */
//     set high order bit of byte;
//   emit byte;
// } while (value != 0);
}

auto encode(T)(T x)
    if (isSigned!T)
{
// more = 1;
// negative = (value < 0);
// size = no. of bits in signed integer;
// while(more) {
//   byte = low order 7 bits of value;
//   value >>= 7;
//   /* the following is unnecessary if the implementation of >>= uses an
//      arithmetic rather than logical shift for a signed left operand */
//   if (negative)
//     value |= - (1 <<(size - 7)); /* sign extend */1

//   /* sign bit of byte is second high order bit (0x40) */
//   if ((value == 0 && sign bit of byte is clear) || (value == -1 && sign bit of byte is set))
//     more = 0;
//   else
//     set high order bit of byte;
//   emit byte;
// }
}

auto decode(T)(T x)
    if (isUnsigned!T)
{
//     result = 0;
// shift = 0;
// while(true) {
//   byte = next byte in input;
//   result |= (low order 7 bits of byte << shift);
//   if (high order bit of byte == 0)
//     break;
//   shift += 7;
// }
}

auto decode(T)(T x)
    if (isSigned!T)
{
//     result = 0;
// shift = 0;
// size = number of bits in signed integer;
// do{
//   byte = next byte in input;
//   result |= (low order 7 bits of byte << shift);
//   shift += 7;
// }while(high order bit of byte != 0);

// /* sign bit of byte is second high order bit (0x40) */
// if ((shift <size) && (sign bit of byte is set))
//   /* sign extend */
//   result |= - (1 << shift);
}
