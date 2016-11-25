/** High-level wrapper for GNU Multiple Precision (MP) library.
 */
module gmp;

/** Arbitrary precision signed integer.
 */
struct Integer
{
    @trusted pure nothrow @nogc:

    @disable this();

    /// Empty (undefined) construction.
    this(typeof(null))
    {
        __gmpz_init(_ptr);
    }

    /// Construct from `value`.
    this(ulong value)
    {
        __gmpz_init_set_ui(_ptr, value);
    }

    /// Destruct.
    ~this()
    {
        if (_ptr) { __gmpz_clear(_ptr); }
    }

    // comparison
    int opCmp(const ref typeof(this) rhs) const { return __gmpz_cmp(_ptr, rhs._ptr); }
    int opCmp(in typeof(this) rhs) const { return __gmpz_cmp(_ptr, rhs._ptr); }
    int opCmp(double rhs) const { return __gmpz_cmp_d(_ptr, rhs); }
    int opCmp(uint rhs) const { return __gmpz_cmp_ui(_ptr, rhs); }
    int opCmp(int rhs) const { return __gmpz_cmp_si(_ptr, rhs); }

    private:

    inout(__mpz_struct)* _ptr() inout { return &_z; }

    __mpz_struct _z;
}

Integer abs(const ref Integer x) @trusted pure nothrow @nogc
{
    typeof(return) y = null;
    __gmpz_abs(y._ptr, x._ptr);
    return y;
}

@safe pure nothrow @nogc unittest
{
    const Integer a = 42;
    Integer b = 43;
    Integer c = null;

    assert(a == a);
    assert(a != b);
    assert(a < b);
    assert(b > a);
    assert(abs(a) == a);

    // Integer a_plus_b = a + b;
    // Integer a_times_b = a * b;
    // assert(a == 42);
}

// C API
extern(C)
{
    struct __mpz_struct
    {
        int _mp_alloc;		/* Number of *limbs* allocated and pointed to by
                                   the _mp_d field.  */
        int _mp_size;           /* abs(_mp_size) is the number of limbs the last
                                   field points to.  If _mp_size is negative
                                   this is a negative number.  */
        void* _mp_d;            /* Pointer to the limbs.  */
    }

    alias mpz_srcptr = const(__mpz_struct)*;
    alias mpz_ptr = __mpz_struct*;

    pure nothrow @nogc:

    void __gmpz_init(mpz_ptr);
    void __gmpz_init_set_ui(mpz_ptr, ulong);

    void __gmpz_clear(mpz_ptr);

    void __gmpz_abs(mpz_ptr, mpz_srcptr);

    int __gmpz_cmp(mpz_srcptr, mpz_srcptr); // TODO: __GMP_NOTHROW __GMP_ATTRIBUTE_PURE;
    int __gmpz_cmp_d(mpz_srcptr, double); // TODO: __GMP_ATTRIBUTE_PURE
    int __gmpz_cmp_si(mpz_srcptr, long); // TODO: __GMP_NOTHROW __GMP_ATTRIBUTE_PURE
    int __gmpz_cmp_ui(mpz_srcptr, ulong); // TODO: __GMP_NOTHROW __GMP_ATTRIBUTE_PURE
}

pragma(lib, "gmp");
