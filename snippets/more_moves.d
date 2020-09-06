// REQUIRED_ARGS: -w -vcolumns -unittest -diagnose=unused

/*
TEST_OUTPUT:
---
fail_compilation/diag_unused_extern.d(18,13): Warning: unused private variable `x1` of module `diag_unused_extern`
---
*/

@safe:

uint postblit;                  // number of calls made to `S.this(this)`

struct S
{
    // @disable this(this);
    this(this) { postblit += 1; }
    // TODO include copy constructor
    int x;
    alias x this;
}

@safe unittest
{
    assert(postblit == 0);

    testA(S.init, 1);
    assert(postblit == 0);

    testC(S.init);
    assert(postblit == 0);

    testAA(S.init);
    assert(postblit == 0);
}

S testA(S e, int x)
{
    return e;                   // moved
}

S testMM(S e, int x)
{
    if (x == 1)
        return e;               // moved
    else
        return e;               // moved
}

S testME(S e, int x)
{
    if (x == 1)
        return e;               // moved
    return e;                   // moved
}

S testV(S e)
{
    S f;
    f = e;                      // single ref of `e` can be moved to `f`
    return f;                   // single ref of `f` can be moved
}

S testC(S e)
{
    auto f = e;                 // single ref of `e` can be moved to `f`
    return f;                   // single ref of `f` can be moved
}

S testAA(S e)
{
    return testA(e, 1);         // TODO: moved
}

S testAAA(S e)
{
    return (e.x == 0 ?
            testA(e, 1) :       // TODO: moved
            testA(e, 1));       // TODO: moved
}

// TODO: Detect cases:
// each VarExp of `e` must be either
// - reads of members (`DotVarExp` where e1 is `e`)
// - pass by move in return statement
// - final assignment
S testB(S e)
{
    if (e.x == 0)               // member read ok
        return testA(e, 1);     // parameter can be passed by move
    return testA(e, 1);         // parameter can be passed by move
}

version(none):

struct A
{
    this(S e)
    {
        this.e = e; // last ref so `e` can be moved
    }
    S e;
}

S testD(S e)
{
    auto f = e;                 // can't move `e`
    auto g = e;                 // can't move `e`
    return f;                   // can move `f`
}

// Can move e because all refs to `e` are direct returns.
S testE(S e)
{
    if (true)
        return e;               // first ref of `e` is moved
    else
        return e;               // second ref of `e` is moved
}

S testF(S e)
{
    auto f = e;                 // can't move `e`
    if (e.x == 0)
        return e;               // can't move `e`
    else if (e.x == 1)
        return e;               // can't move `e`
    else
        return f;               // can move `f`
}