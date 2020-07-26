#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <chrono>

#include <typeinfo>
#include <cxxabi.h>

#include "flat_hash_map.hpp"
#include "robin_hood.h"

using namespace std;

// save some typing
namespace cr = chrono;

// you can replace this with steady_clock or system_clock
using Clock = cr::high_resolution_clock;

template<class Duration>
void showTime(const string& tag, Duration dur, size_t elementCount, bool okFlag)
{
    const auto dur_ns = cr::duration_cast<cr::nanoseconds>(dur).count();
    cout << tag << ": "
         << (static_cast<double>(dur_ns)) / elementCount << " nsecs/op "
         << (okFlag ? "OK" : "ERR")
         << ", ";
}

template<class T>
void showHeader()
{
    int status;
    const auto name = abi::__cxa_demangle(typeid(T).name(), 0, 0, &status);
    cout << name << ":" << endl;
}

template<class Vector>
void benchmarkVector(size_t elementCount)
{
    showHeader<Vector>();
    Vector x;
    x.reserve(elementCount);

    const auto beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        x.push_back(i);
    }
    const auto end = Clock::now();
    showTime("push_back", end - beg, elementCount, true);
    cout << endl << endl;
}

template<class Set>
void benchmarkSet(size_t elementCount)
{
    showHeader<Set>();
    Set x;
    x.reserve(elementCount);

    auto beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        x.insert(i);
    }
    auto end = Clock::now();
    showTime("insert", end - beg, elementCount, true);

    bool allHit = true;
    beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        const auto hit = x.find(i);
        if (hit == x.end()) { allHit = false; }
    }
    end = Clock::now();

    showTime("find", end - beg, elementCount, allHit);
    cout << endl << endl;
    x.clear();
}

template<class Map>
void benchmarkMap(size_t elementCount)
{
    showHeader<Map>();
    Map x;
    x.reserve(elementCount);

    auto beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        x.insert(make_pair(i, i));
    }
    auto end = Clock::now();
    showTime("insert", end - beg, elementCount, true);

    bool allHit = true;
    beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        const auto hit = x.find(i);
        if (hit == x.end()) { allHit = false; }
    }
    end = Clock::now();

    showTime("find", end - beg, elementCount, allHit);
    cout << endl << endl;
    x.clear();
}

int main(__attribute__((unused)) int argc,
         __attribute__((unused)) const char* argv[],
         __attribute__((unused)) const char* envp[])
{
    typedef ulong E;
    const size_t elementCount = 400000;

    cout << fixed << setprecision(3);

    cout << "# Vector:" << endl;
    benchmarkVector<std::vector<E>>(elementCount);

    cout << "# Sets:" << endl;
    benchmarkSet<ska::flat_hash_set<E>>(elementCount);
    benchmarkSet<robin_hood::unordered_flat_set<E>>(elementCount);
    benchmarkSet<robin_hood::unordered_node_set<E>>(elementCount);
    benchmarkSet<robin_hood::unordered_set<E>>(elementCount);
    benchmarkSet<std::unordered_set<E>>(elementCount);

    cout << "# Maps:" << endl;
    benchmarkMap<ska::flat_hash_map<E, E>>(elementCount);
    benchmarkMap<std::unordered_map<E, E>>(elementCount);

    return 0;
}
