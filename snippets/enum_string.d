alias StorageClass = ulong;

enum STC : StorageClass
{
    undefined_          = 0L,
    static_             = (1L << 0),
    extern_             = (1L << 1),
    const_              = (1L << 2),
    final_              = (1L << 3),
    abstract_           = (1L << 4),
    parameter           = (1L << 5),
    field               = (1L << 6),
    override_           = (1L << 7),
    auto_               = (1L << 8),
    synchronized_       = (1L << 9),
    deprecated_         = (1L << 10),
    in_                 = (1L << 11),   // in parameter
    out_                = (1L << 12),   // out parameter
    lazy_               = (1L << 13),   // lazy parameter
    foreach_            = (1L << 14),   // variable for foreach loop
                          //(1L << 15)
    variadic            = (1L << 16),   // the 'variadic' parameter in: T foo(T a, U b, V variadic...)
    ctorinit            = (1L << 17),   // can only be set inside constructor
    templateparameter   = (1L << 18),   // template parameter
    scope_              = (1L << 19),
    immutable_          = (1L << 20),
    ref_                = (1L << 21),
    init                = (1L << 22),   // has explicit initializer
    manifest            = (1L << 23),   // manifest constant
    nodtor              = (1L << 24),   // don't run destructor
    nothrow_            = (1L << 25),   // never throws exceptions
    pure_               = (1L << 26),   // pure function
    tls                 = (1L << 27),   // thread local
    alias_              = (1L << 28),   // alias parameter
    shared_             = (1L << 29),   // accessible from multiple threads
    gshared             = (1L << 30),   // accessible from multiple threads, but not typed as "shared"
    wild                = (1L << 31),   // for "wild" type constructor
    property            = (1L << 32),
    safe                = (1L << 33),
    trusted             = (1L << 34),
    system              = (1L << 35),
    ctfe                = (1L << 36),   // can be used in CTFE, even if it is static
    disable             = (1L << 37),   // for functions that are not callable
    result              = (1L << 38),   // for result variables passed to out contracts
    nodefaultctor       = (1L << 39),   // must be set inside constructor
    temp                = (1L << 40),   // temporary variable
    rvalue              = (1L << 41),   // force rvalue for variables
    nogc                = (1L << 42),   // @nogc
    volatile_           = (1L << 43),   // destined for volatile in the back end
    return_             = (1L << 44),   // 'return ref' or 'return scope' for function parameters
    autoref             = (1L << 45),   // Mark for the already deduced 'auto ref' parameter
    inference           = (1L << 46),   // do attribute inference
    exptemp             = (1L << 47),   // temporary variable that has lifetime restricted to an expression
    maybescope          = (1L << 48),   // parameter might be 'scope'
    scopeinferred       = (1L << 49),   // 'scope' has been inferred and should not be part of mangling
    future              = (1L << 50),   // introducing new base class function
    local               = (1L << 51),   // do not forward (see dmd.dsymbol.ForwardingScopeDsymbol).
    returninferred      = (1L << 52),   // 'return' has been inferred and should not be part of mangling
    live                = (1L << 53),   // function @live attribute

    // Group members are mutually exclusive (there can be only one)
    safeGroup = STC.safe | STC.trusted | STC.system,

    /// Group for `in` / `out` / `ref` storage classes on parameter
    IOR  = STC.in_ | STC.ref_ | STC.out_,

    TYPECTOR = (STC.const_ | STC.immutable_ | STC.shared_ | STC.wild),
    FUNCATTR = (STC.ref_ | STC.nothrow_ | STC.nogc | STC.pure_ | STC.property | STC.live |
                STC.safeGroup),
}

string toStringOfSTCs(StorageClass storage_class) pure nothrow @safe
{
    string result;
    static foreach (i, element; __traits(allMembers, STC))
    {
        if (element != "safeGroup" &&
            element != "IOR" &&
            element != "TYPECTOR" &&
            element != "FUNCATTR")
        {
            enum stc = mixin("STC.", element);
            if (storage_class & stc)
            {
                if (result)
                    result ~= ",";
                result ~= element;
            }
            pragma(msg, stc);
        }

    }
    return result;
}

@safe pure unittest
{
    const StorageClass storage_class = STC.future | STC.live;
    assert(toStringOfSTCs(storage_class) == "future,live");
}
