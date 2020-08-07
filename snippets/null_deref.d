class Name
{
@safe:
    this()
    {
    }
    bool get() { return _x; }
    bool _x;
}

@safe pure unittest
{
    Name n;
    bool x = n.get(3);          // dmd.func.resolveFuncCall
}
