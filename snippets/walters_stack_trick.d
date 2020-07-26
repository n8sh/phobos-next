@safe pure unittest
{
    enum n = 100;
    uint[n] x;                  // stack allocated
    const length = n;
    auto y = x[0 .. length + 1]; // range violation with
}
