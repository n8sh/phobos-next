#include <iostream>
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
    cout << "insert: " << (static_cast<double>(dur_ns)) / elementCount << " nsecs/op "
         << (okFlag ? "OK" : "ERR");
}

template<class Vector>
void benchmarkVector(size_t elementCount)
{
    int status;
    const auto name = abi::__cxa_demangle(typeid(Vector).name(), 0, 0, &status);
    cout << name << ":" << endl;
    Vector a;
    a.reserve(elementCount);
    const auto beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        a.push_back(i);
    }
    const auto end = Clock::now();
    const auto diff = end - beg;
    cout << "push_back: "
         << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op\n";
}

template<class Set>
void benchmarkSet(size_t elementCount)
{
    Set us;
    us.reserve(elementCount);
    int status;
    const auto name = abi::__cxa_demangle(typeid(Set).name(), 0, 0, &status);
    cout << name << ":" << endl;
    {
        const auto beg = Clock::now();
        for (size_t i = 0; i < elementCount; ++i)
        {
            us.insert(i);
        }
        const auto end = Clock::now();
        showTime("insert", end - beg, elementCount, true);
    }
    {
        bool allHit = true;
        const auto beg = Clock::now();
        for (size_t i = 0; i < elementCount; ++i)
        {
            const auto hit = us.find(i);
            if (hit == us.end()) { allHit = false; }
        }
        const auto end = Clock::now();
        showTime("find", end - beg, elementCount, allHit);
    }
    cout << endl << endl;
    us.clear();
}

template<class Map>
void benchmarkMap(size_t elementCount)
{
    Map us;
    us.reserve(elementCount);
    int status;
    const auto name = abi::__cxa_demangle(typeid(Map).name(), 0, 0, &status);
    cout << name << ":" << endl;
    {
        const auto beg = Clock::now();
        for (size_t i = 0; i < elementCount; ++i)
        {
            us.insert(make_pair(i, i));
        }
        const auto end = Clock::now();
        showTime("insert", end - beg, elementCount, true);
    }
    {
        bool allHit = true;
        const auto beg = Clock::now();
        for (size_t i = 0; i < elementCount; ++i)
        {
            const auto hit = us.find(i);
            if (hit == us.end()) { allHit = false; }
        }
        const auto end = Clock::now();
        showTime("find", end - beg, elementCount, allHit);
    }
    cout << endl << endl;
    us.clear();
}

int main(int argc, const char* argv[], const char* envp[])
{
    typedef ulong E;
    const size_t elementCount = 400000;

    benchmarkVector<vector<E>>(elementCount);

    benchmarkSet<std::unordered_set<E>>(elementCount);
    benchmarkSet<ska::flat_hash_set<E>>(elementCount);
    benchmarkSet<robin_hood::unordered_flat_set<E>>(elementCount);
    benchmarkSet<robin_hood::unordered_node_set<E>>(elementCount);
    benchmarkSet<robin_hood::unordered_set<E>>(elementCount);

    benchmarkMap<unordered_map<E, E>>(elementCount);
    benchmarkMap<ska::flat_hash_map<E, E>>(elementCount);

    return 0;
}
