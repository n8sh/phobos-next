module uncopyable_sample;

struct V
{
    import qcmeman : malloc, free;

    this(uint i)
    {
        _i = cast(typeof(_i))malloc(1 * (*_i).sizeof);
        dln("allocated: ", _i, " being ", *_i);
    }

    @disable this(this);

    ~this()
    {
        if (_i)
        {
            dln("freeing: ", _i, " being ", *_i);
        }
        free(_i);
    }

    uint *_i;
}

version(unittest)
{
    import dbgio;
}
