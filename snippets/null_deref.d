@safe pure unittest
{
    class Name
    {
    @safe:
        this()
        {
        }
        bool get() { return _x; }
        bool _x;
    }
    Name n;
    bool x = n.get;
}
