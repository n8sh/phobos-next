@safe pure unittest
{
	struct S { int i; float f; double d; }
    // alias T = S.tupleof;
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", typeof(S.tupleof));
}
