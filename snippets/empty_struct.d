/** Empty structs have size zero.
 *
 * See_Also: https://dlang.org/spec/struct.html#struct_layout
 */

struct S
{
}

extern(C) struct CS
{
}

@safe pure unittest
{
    static assert(S.tupleof.length == 0);
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", S.sizeof);
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", CS.sizeof);
}
