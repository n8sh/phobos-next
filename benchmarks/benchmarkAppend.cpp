#include <iostream>
#include <string>
#include <vector>
#include <chrono>

using std::cout;
using std::endl;
using std::hex;
using std::dec;

int main(int argc, const char* argv[], const char* envp[])
{
    // save some typing
    namespace cr = std::chrono;

    // you can replace this with steady_clock or system_clock
    typedef cr::high_resolution_clock my_clock;

    // get the clock time before operation.
    // note that this is a static function, and
    // we don't actually create a clock object

    typedef int E;
    std::vector<int> a;
    const uint n = 5000000;
    a.reserve(n);

    const auto start_time = my_clock::now();
    for (size_t i = 0; i < n; ++i)
    {
        a.push_back(i);
    }
    const auto end_time = my_clock::now();

    // get the elapsed time
    const auto diff = end_time - start_time;

    // convert from the clock rate to a millisecond clock
    const auto microseconds = cr::duration_cast<cr::microseconds>(diff);

    // get the clock count (i.e. the number of microseconds)
    const auto millisecond_count = microseconds.count();

    std::cout << millisecond_count << '\n';

    return 0;
}
