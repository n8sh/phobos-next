@safe pure unittest
{
    int x;
    x = x;                      // warn

    int y;
    y = x;                      // no warn

    y = 32;                     // no warn
}
