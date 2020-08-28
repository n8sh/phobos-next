import core.stdc.stdarg;

struct Loc { int x; }

pragma(printf) extern (C) int warning(Loc, const(char)*, ...) { return 0; }

unittest
{
    Loc loc;
    loc.warning("%d", 32);      // ok
    loc.warning("%p", null);    // ok
    loc.warning("%s", "".ptr);  // ok
    loc.warning("%s", 32);      // warn
}
