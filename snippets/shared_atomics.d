shared int x;
shared int y;
shared int* ptr;
shared static this() { ptr = new int; } // silence null-dereference errors
class NS { shared int x; }
shared class S { int sx; }

version(none)
void err()
{
    ++x;
    x++;
    --x;
    x--;
    x += 1;
    x += 2;
    x -= 3;
    x |= y;
    x *= y;
    x /= y;
    x %= y;
    x &= y;
    x ^= y;
    x <<= y;
    x >>= y;
    x >>>= y;
    x ^^= y;
    ++ptr;
    ptr++;
    --ptr;
    ptr--;
    NS ns = new NS;
    ns.x++;
    S s = new S;
    s.sx++;
}

void ok()
{
    import core.atomic : atomicOp;
    import std.stdio : writeln;
    writeln(x);
    atomicOp!"+="(x, 1);
    writeln(x);
    atomicOp!"*="(x, 2);
    writeln(x);
    atomicOp!"/="(x, 2);
    writeln(x);
}

void main()
{
    ok();
}
