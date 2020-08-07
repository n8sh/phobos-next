class A
{
@safe pure:
    this() {}
    bool get() { return _x; }
    bool _x;
}

class B : A
{
@safe pure:
    this() {}
    override bool get() { return _x; }
    bool _x;
}

@safe pure unittest
{
    A a;
    B b = a;
}
