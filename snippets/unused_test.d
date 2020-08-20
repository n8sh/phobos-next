module unused_test;

void foo() {}

@safe pure unittest
{
    alias Int = int;
    Int x;
}
