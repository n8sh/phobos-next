module hashfuns;

uint muellerHash32(uint x)
{
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

/** Hash

   Based on splitmix64, which seems to be based on the blog article "Better Bit
   Mixing" (mix 13).

   See also: https://stackoverflow.com/a/12996028/683710
   See also: http://zimbry.blogspot.se/2011/09/better-bit-mixing-improving-on.html
 */
ulong muellerHash64(ulong x)
{
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9UL;
    x = (x ^ (x >> 27)) * 0x94d049bb133111ebUL;
    x = x ^ (x >> 31);
    return x;
}
