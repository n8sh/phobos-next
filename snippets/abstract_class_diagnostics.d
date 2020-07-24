@safe:

class C
{
    abstract void f();
}

@safe pure unittest
{
    scope c = new C();
}
