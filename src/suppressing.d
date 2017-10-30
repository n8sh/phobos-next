/** Various suppressing hacks.
 */
module suppressing;

enum SuppressOptions
{
    destructor = 1,
    postblit = 2
}

/** Suppres.
 *
 * See also: http://forum.dlang.org/post/dxakoknmzblxpgiibfmu@forum.dlang.org
 */
struct Suppress(T, SuppressOptions options)
    if (options != 0)
{
    import std.traits : isCopyable;
    private enum suppressPostblit   = (options & SuppressOptions.postblit)   != 0;
    private enum suppressDestructor = (options & SuppressOptions.destructor) != 0;
    private enum postblitName = __traits(hasMember, T, "__xpostblit") ? "__xpostblit" : "__postblit";

    // Disguise T as a humble array.
    private ubyte[T.sizeof] _payload;

    // Create from instance of T.
    this(T arg)
    {
        _payload = *cast(ubyte[T.sizeof]*)&arg;
    }

    // Or forward constructor arguments to T's constructor.
    static if (__traits(hasMember, T, "__ctor"))
    {
        this(Args...)(Args args)
            if (__traits(compiles, (Args e){__traits(getMember, T.init, "__ctor")(e);}))
        {
            __traits(getMember, get, "__ctor")(args);
        }
    }

    // Call dtor
    static if (!suppressDestructor)
    {
        ~this()
        {
            destroy(get);
        }
    }

    // Call postblit
    static if (!suppressPostblit)
    {
        static if (!isCopyable!T)
        {
            @disable this(this);
        }
        else static if (__traits(hasMember, T, postblitName))
        {
            this(this)
            {
                __traits(getMember, get, postblitName)();
            }
        }
    }

    // Pretend to be a T.
    @property
    ref T get()
    {
        return *cast(T*)_payload.ptr;
    }

    alias get this;
}

struct S1
{
    @disable this(this);
    ~this()
    {
        throw new Exception("Don't touch my destructor!");
    }
}

unittest
{
    import std.exception;
    static assert(!__traits(compiles, (Suppress!S1 a) { auto b = a; }));
    static assert(__traits(compiles, (Suppress!(S1, SuppressOptions.postblit) a) { auto b = a; }));

    assertThrown({ Suppress!(S1, SuppressOptions.postblit) a; }());
    assertNotThrown({ Suppress!(S1, SuppressOptions.postblit | SuppressOptions.destructor) a; }());
}
