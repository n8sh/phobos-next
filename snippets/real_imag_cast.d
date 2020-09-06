@safe pure unittest
{
    idouble x = 2.0i;
    double y = cast(double)x; // y is now 0
    assert(y == 0);
}
