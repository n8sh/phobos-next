/*! \file f.cpp
 * \brief
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

int f(int x)
{
    return f(x);
}    

int g(int x)
{
    if (x)
        return f(x) * f(x + 1);
    else
        return 42;
}    

int a(int x);
int b(int x);

int a(int x)
{
    return b(x);
}    

int b(int x)
{
    return a(x);
}    

int main(__attribute__((unused)) int argc,
         __attribute__((unused)) const char * argv[],
         __attribute__((unused)) const char * envp[])
{
    int x = 0;
    x = x;
    return 0;
}
