/*! \file t_dangling.cpp
 * \brief
 */

#include <iostream>
#include <string>

using std::cout;
using std::endl;
using std::hex;
using std::dec;

int* id(int* a)
{
    return a;
}


int* dangling()
{
    int i = 1234;
    return id(&i);
}

int add_one()
{
    int* num = dangling();
    return *num + 1;
}

int main(int argc, const char * argv[], const char * envp[])
{
    add_one();
    return 0;
}
