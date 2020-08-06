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
    Name x;
    assert(x.get == false);
}
