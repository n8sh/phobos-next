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

template staticSortByDescendingAlignment(Ts...)
{
    import std.meta : staticSort;
    alias staticSortByDescendingAlignment = staticSort!(alignofComp, Ts);
    enum alignofComp(alias A, alias B) = A.alignof > B.alignof;
}

@safe pure unittest
{
    import std.meta : AliasSeq;
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", S.sizeof);
    alias T = AliasSeq!(staticSortByDescendingAlignment!(typeof(S.tupleof)));
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", T, " of size ", T.sizeof);
}

unittest {

}
