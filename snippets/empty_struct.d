struct S
{
    bool x;
}

extern(C) struct CS
{
    bool x;
}

@safe pure unittest
{
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", S.sizeof);
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", CS.sizeof);
}
