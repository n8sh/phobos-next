@safe pure unittest
{
	class C
    {
        void f(scope C b) @safe pure 
        {
            b = this;           // passes because f is not `scope`
        }
    }
}

@safe pure unittest
{
	class C
    {
        scope void f(scope C b) @safe pure 
        {
            b = this;           // fails because of `scope` qualifier
        }
    }
}
