/*! \file f.cpp
 *
 * \brief Clang can detect self-assignment of variables. My solution is more
 * sensitive and specialize diagnostics on presence of postblit or/and copy
 * constructor.
 *
 * \see https://github.com/dlang/dmd/pull/11553
 */

#include <iostream>
#include <string>

using std::cout;
using std::endl;
using std::hex;
using std::dec;

class C
{
    C(int x)
    {
        x = x;
        _x = _x;
    }
    int _x;
};

int f1(int x)
{
    return f1(x);
}

int f2(int x)
{
    if (x == 0)
        return f2(x);
    return 1;
}

int g(int x)
{
    if (x)
        return f1(x) * f1(x + 1);
    else
        return 42;
}

int a(int x);
int b(int x);
int a(int x) { return b(x); }  // mutual unconditional recursion is not detected
int b(int x) { return a(x); }  // mutual unconditional recursion is not detected

int main(__attribute__((unused)) int argc,
         __attribute__((unused)) const char * argv[],
         __attribute__((unused)) const char * envp[])
{
    int x = 0;
    x = x;
    return 0;
}
