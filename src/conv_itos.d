module conv_itos;

@safe:

uint fastLog10(const uint val) @safe pure nothrow @nogc
{
    // in order of probability
    if (val < 1e1) return 0;
    if (val < 1e2) return 1;
    if (val < 1e3) return 2;
    if (val < 1e4) return 3;
    if (val < 1e5) return 4;
    if (val < 1e6) return 5;
    if (val < 1e7) return 6;
    if (val < 1e8) return 7;
    if (val < 1e9) return 8;
    if (val < 1e10) return 9;
    return 9;
}

@safe pure unittest
{
    assert(fastLog10(1) == 0);
    assert(fastLog10(9) == 0);
    assert(fastLog10(11) == 1);
    assert(fastLog10(99) == 1);
    assert(fastLog10(111) == 2);
    assert(fastLog10(999) == 2);
    assert(fastLog10(1_111) == 3);
    assert(fastLog10(9_999) == 3);
    assert(fastLog10(11_111) == 4);
    assert(fastLog10(99_999) == 4);
    assert(fastLog10(999_999_999) == 8);
    assert(fastLog10(1_000_000_000) == 9);
    assert(fastLog10(uint.max) == 9);
}

/*@unique*/
private static immutable fastPow10tbl_32bit_unsigned = [
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
    ];

string uint_to_string(const uint val) @trusted pure nothrow
{
    immutable length = fastLog10(val) + 1;
    char[] result;
    result.length = length;
    foreach (immutable i; 0 .. length)
    {
        immutable _val = val / fastPow10tbl_32bit_unsigned[i];
        result[length - i - 1] = cast(char)((_val % 10) + '0');
    }
    return cast(string) result;
}

static assert(mixin(uint.max.uint_to_string) == uint.max);

@safe pure unittest
{
}
