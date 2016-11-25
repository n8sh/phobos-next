/** High-level wrapper for GNU Multiple Precision (MP) library.
 */
module gmp;

struct Z
{
    pure nothrow @nogc:

    this(ulong value) @trusted
    {
        __gmpz_init_set_ui(_z, value);
    }

    ~this() @trusted
    {
        if (_z) { __gmpz_clear(_z); }
    }

    private __mpz_struct* _z;
}

@safe pure nothrow @nogc unittest
{
    Z z;
}

// C API
extern(C)
{
    struct __mpz_struct;
    alias mpz_srcptr = const(__mpz_struct)*;
    alias mpz_ptr = __mpz_struct*;

    pure nothrow @nogc:

    void __gmpz_init(mpz_ptr);
    void __gmpz_init_set_ui(mpz_ptr, ulong);

    void __gmpz_clear(mpz_ptr);
}

pragma(lib, "gmp");
