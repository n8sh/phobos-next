@safe pure unittest
{
    int x = 0;
    int y = 1;
    if (x == 0 && (x + 1) == (y + 2))
    {
        x += 1;
        y += 1;
    }
}
