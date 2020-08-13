@safe pure unittest
{
    enum E { e }
    enum D { d }
    assert(E.e == D.d);
}
