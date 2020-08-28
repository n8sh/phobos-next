import core.stdc.stdarg;

struct Loc { int x; }

//pragma(printf) extern (D) int warningD(in Loc, const(char)*, ...) { return 0; }
pragma(printf) extern (C) int warningC(in Loc, const(char)*, ...) { return 0; }
pragma(printf) extern (C++) int warningCxx(in Loc, const(char)*, ...) { return 0; }

unittest
{
    Loc loc;
    loc.warningCxx("%d", 32);      // ok
    loc.warningCxx("%p", null);    // ok
    loc.warningCxx("%s", "".ptr);  // ok
    loc.warningCxx("%s", 32);      // warn
}
