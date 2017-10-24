/** Various suppressing hacks.
 */
module suppressing;

/** Wrapper type for `T` with postblit disabled.
 */
struct SuppressPostblit(T)
{
    /// Disguise `T` as a humble array.
    private ubyte[T.sizeof] _payload;

    /// Create from instance of `T`.
    this(T arg)
    {
        _payload = *cast(ubyte[T.sizeof]*)&arg;
    }

    /// Or forward constructor arguments to `T`'s constructor.
    static if (__traits(hasMember, T, "__ctor"))
    {
        this(Args...)(Args args)
        if (__traits(compiles, (Args e){__traits(getMember, T.init, "__ctor")(e);}))
        {
            __traits(getMember, get, "__ctor")(args);
        }
    }

    /// Pretend to be a `T`.
    @property
    ref inout(T) get() inout
    {
        return *cast(typeof(return)*)_payload.ptr;
    }

    alias get this;

    static if (__traits(hasMember, T, "__dtor"))
    {
        ~this()
        {
            __traits(getMember, get, "__dtor")();
        }
    }
}
