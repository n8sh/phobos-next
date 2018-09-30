import core.stdc.stdio: printf;
import std.stdio;

void main(string[] args)
{
    // writeln("args:", args);
    const n = 10;
    foreach (i; 0 .. n)
    {
        int* x = new int(i);
        printf("i:%d, p:%p\n", i, x);
    }
}
