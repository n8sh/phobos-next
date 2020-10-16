// See_Also: https://forum.dlang.org/post/tbqjhxebbczvbltvadvd@forum.dlang.org

struct S
{
    int i;
    bool b;
}

struct T
{
    S s;
    char c;
}

pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", T.sizeof);

struct U
{
    int i;
    bool b;
    char c;
}

pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", U.sizeof);
