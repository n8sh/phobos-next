@safe pure unittest
{
    idouble x = 2.0i;
    double y = cast(double)x; // TODO: should warn
    assert(y == 0);
}

@safe pure unittest
{
    double x = 2.0;
    idouble y = cast(idouble)x; // TODO: should warn
    assert(y == 0);
}
