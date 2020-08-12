@safe pure nothrow @nogc:

int f() { return 42; }

@safe pure unittest
{
    f();
}
