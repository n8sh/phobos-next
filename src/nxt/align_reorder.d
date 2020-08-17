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

private static enum alignOf(T) = T.alignof;

template sortBy(alias pred, Ts...)
{
    import std.meta : AliasSeq;
    static if (Ts.length <= 1)
        alias sortBy = Ts;
    else
    {
        static if (pred!(Ts[0]) <
                   pred!(Ts[1]))
            alias sortBy = AliasSeq!(Ts[0], Ts[1], sortBy!(Ts[2 .. $]));
        else
            alias sortBy = AliasSeq!(Ts[1], Ts[0], sortBy!(Ts[2 .. $]));
    }
}

@safe pure unittest
{
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", S.sizeof);
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", sortBy!(alignOf, typeof(S.tupleof)));
}
