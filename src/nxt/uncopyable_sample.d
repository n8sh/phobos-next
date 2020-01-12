module nxt.uncopyable_sample;

struct SomeUncopyable
{
    import nxt.qcmeman : malloc, free;

    import nxt.dbgio;

    @safe pure nothrow @nogc:

    this(uint i) @trusted
    {
        _i = cast(typeof(_i))malloc(1 * (*_i).sizeof);
        *_i = i;
        dbg("allocated: ", _i, " being ", *_i);
    }

    @disable this(this);

    ~this() @trusted @nogc
    {
        if (_i)
        {
            // dbg("freeing: ", _i, " being ", *_i);
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
