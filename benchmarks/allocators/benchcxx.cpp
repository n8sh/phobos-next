#include <iostream>
#include <chrono>
#include <string>
#include <sys/time.h>

using namespace std;
using namespace std::chrono;

int main()
{
    const auto value = "alpha_beta_gamma_delta";
    const auto count = 1000000;
    high_resolution_clock::time_point t1 = high_resolution_clock::now();
    const char* iptr = nullptr;
    for (auto i = 0; i < count; ++i)
    {
        auto x = new string(value);
        iptr = x->c_str();
    }
    high_resolution_clock::time_point t2 = high_resolution_clock::now();
    const double dif = static_cast<double>(duration_cast<nanoseconds>( t2 - t1 ).count()) / count;
    cout << "Allocating " << count << " string took " << dif << " ns/string";
    return 0;
}
