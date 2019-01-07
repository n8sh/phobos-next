module conv_itos;

@safe:

const(uint) fastLog10(const uint val) @safe pure nothrow @nogc
{
    return (val < 10) ? 0 : (val < 100) ? 1 : (val < 1000) ? 2 : (val < 10000) ? 3
    : (val < 100000) ? 4 : (val < 1000000) ? 5 : (val < 10000000) ? 6
    : (val < 100000000) ? 7 : (val < 1000000000) ? 8 : 9;
}

/*@unique*/
private static immutable fastPow10tbl_32bit_unsigned = [
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
    ];

string itos(const uint val) @trusted pure nothrow
{
    immutable length = fastLog10(val) + 1;
    char[] result;
    result.length = length;

    foreach (i; 0 .. length)
    {
        immutable _val = val / fastPow10tbl_32bit_unsigned[i];
        result[length - i - 1] = cast(char)((_val % 10) + '0');
    }

    return cast(string) result;
}

static assert(mixin(uint.max.itos) == uint.max);

@safe pure unittest
{
}
