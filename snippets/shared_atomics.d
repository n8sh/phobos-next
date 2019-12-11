import core.atomic : atomicOp;

shared int x;
shared int y;
shared int* ptr;
shared static this() { ptr = new int; } // silence null-dereference errors
class NS { shared int x; }
shared class S { int sx; }

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
}
