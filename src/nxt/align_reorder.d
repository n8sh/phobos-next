/** Reorder aggregate tupleof by descending alignment.
 */
module nxt.align_reorder;

struct S
{
    bool x;
    bool y;
    int i;
    bool z;
}

template realigned(T)
{
    alias realigned = typeof(T.tupleof);
}

@safe pure unittest
{
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", S.sizeof);
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", realigned!(S));
}
