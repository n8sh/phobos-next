// Compile with: `gdc-10 -Wcast-result real_imag_cast.d`

@safe pure test1()
{
    idouble x = 2.0i;
    const double y = cast(double)x; // TODO: should warn
    assert(y == 0);
}

@safe pure test2()
{
    double x = 2.0;
    const idouble y = cast(idouble)x; // TODO: should warn
    assert(y == 0);
}
