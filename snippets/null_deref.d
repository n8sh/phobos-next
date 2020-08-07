class Name
{
@safe pure:
    this()
    {
    }
    bool get() { return _x; }
    bool _x;
}

@safe pure unittest
{
    Name n;
    bool x = n.get();           // dmd.func.resolveFuncCall
}
