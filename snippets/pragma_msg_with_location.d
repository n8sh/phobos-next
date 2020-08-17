@safe pure unittest
{
    int x;
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", typeof(x));
}
