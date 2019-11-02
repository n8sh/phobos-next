@safe:

class C
{
    abstract void f();
}

@safe pure unittest
{
    auto c = new C();
}
