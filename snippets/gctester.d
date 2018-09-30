import core.stdc.stdio: printf;
import std.stdio;

void main(string[] args)
{
    writeln("args:", args);
    int* x = new int(32);
    printf("p:%p\n", x);
}
