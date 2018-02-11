module uncopyable_sample;

struct SomeUncopyable
{
    import qcmeman : malloc, free;

    import dbgio;

    @safe pure nothrow @nogc:

    this(uint i) @trusted
    {
        _i = cast(typeof(_i))malloc(1 * (*_i).sizeof);
        *_i = i;
        dln("allocated: ", _i, " being ", *_i);
    }

    @disable this(this);

    ~this() @trusted
    {
        if (_i)
        {
            dln("freeing: ", _i, " being ", *_i);
        }
        free(_i);
    }

    inout(uint)* valuePointer() inout { return _i; }

    typeof(this) dup()
    {
        return typeof(return)(*_i);
    }

    uint *_i;
}
