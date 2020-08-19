@safe pure unittest
{
    bool x;
    int i;
    if ((x && x) ||
        (x && x))
        i = 42;
}
