#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <set>
#include <map>
#include <chrono>

#include <typeinfo>
#include <cxxabi.h>

#include "flat_hash_map.hpp"
#include "bytell_hash_map.hpp"
#include "robin_hood.h"

// https://github.com/Tessil/robin-map
#include "tsl/robin_set.h"
#include "tsl/robin_map.h"

#include "has_member.hpp"
define_has_member(reserve);

using namespace std;

using E = ulong;
using UlongArray = std::vector<E>;

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
void benchmarkVector(size_t elementCount,
                     const UlongArray& ulongArray)
{
    showHeader<Vector>();
    Vector x;
    if constexpr (has_member(Vector, reserve))
    {
        x.reserve(elementCount);
    }

    auto beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        x.push_back(ulongArray[i]);
    }
    auto end = Clock::now();
    showTime("push_back", end - beg, elementCount, true);
    cout << endl << endl;
}

template<class Set>
void benchmarkSet(size_t elementCount,
                  const UlongArray& ulongArray)
{
    showHeader<Set>();
    Set x;
    if constexpr (has_member(Set, reserve))
    {
        x.reserve(elementCount);
    }

    auto beg = Clock::now();
    for (const auto& e : ulongArray)
    {
        x.insert(e);
    }
    auto end = Clock::now();
    showTime("insert", end - beg, elementCount, true);

    bool allHit = true;
    beg = Clock::now();
    for (const auto& e : ulongArray)
    {
        const auto hit = x.find(e);
        if (hit == x.end()) { allHit = false; }
    }
    end = Clock::now();
    showTime("find", end - beg, elementCount, allHit);

    bool allErase = true;
    beg = Clock::now();
    for (const auto& e : ulongArray)
    {
        const auto count = x.erase(e);
        if (count != 1) { allErase = false; }
    }
    end = Clock::now();
    showTime("erase", end - beg, elementCount, allErase);

    cout << endl << endl;
    x.clear();
}

template<class Map>
void benchmarkMap(size_t elementCount,
                  const UlongArray& ulongArray)
{
    showHeader<Map>();
    Map x;
    if constexpr (has_member(Map, reserve))
    {
        x.reserve(elementCount);
    }

    auto beg = Clock::now();
    for (const auto& e : ulongArray)
    {
        x[e] = e;
    }
    auto end = Clock::now();
    showTime("insert", end - beg, elementCount, true);

    bool allHit = true;
    beg = Clock::now();
    for (const auto& e : ulongArray)
    {
        const auto hit = x.find(e);
        if (hit == x.end()) { allHit = false; }
    }
    end = Clock::now();
    showTime("find", end - beg, elementCount, allHit);

    bool allErase = true;
    beg = Clock::now();
    for (const auto& e : ulongArray)
    {
        const auto count = x.erase(e);
        if (count != 1) { allErase = false; }
    }
    end = Clock::now();
    showTime("erase", end - beg, elementCount, allErase);


    cout << endl << endl;
    x.clear();
}

int main(__attribute__((unused)) int argc,
         __attribute__((unused)) const char* argv[],
         __attribute__((unused)) const char* envp[])
{
    const size_t elementCount = 400000;

    UlongArray ulongArray(elementCount);
    for (size_t i = 0; i < elementCount; ++i)
    {
        ulongArray[i] = i;
    }
    std::random_shuffle(begin(ulongArray),
                        end(ulongArray));

    cout << fixed << setprecision(3);

    cout << "# Vector:" << endl;
    benchmarkVector<std::vector<E>>(elementCount, ulongArray);

    cout << "# Unordered Sets:" << endl;
    benchmarkSet<tsl::robin_set<E>>(elementCount, ulongArray);
    benchmarkSet<tsl::robin_pg_set<E>>(elementCount, ulongArray);
    benchmarkSet<ska::flat_hash_set<E>>(elementCount, ulongArray);
    /* TODO benchmarkSet<ska::bytell_hash_set<E>>(elementCount, ulongArray); */
    benchmarkSet<robin_hood::unordered_flat_set<E>>(elementCount, ulongArray);
    benchmarkSet<robin_hood::unordered_node_set<E>>(elementCount, ulongArray);
    benchmarkSet<robin_hood::unordered_set<E>>(elementCount, ulongArray);
    benchmarkSet<std::unordered_set<E>>(elementCount, ulongArray);

    cout << "# Ordered Sets:" << endl;
    benchmarkSet<std::set<E>>(elementCount, ulongArray);

    cout << "# Unordered Maps:" << endl;
    benchmarkMap<tsl::robin_map<E, E>>(elementCount, ulongArray);
    benchmarkMap<tsl::robin_pg_map<E, E>>(elementCount, ulongArray);
    benchmarkMap<ska::flat_hash_map<E, E>>(elementCount, ulongArray);
    /* TODO benchmarkMap<ska::bytell_hash_map<E, E>>(elementCount, ulongArray); */
    benchmarkMap<robin_hood::unordered_flat_map<E, E>>(elementCount, ulongArray);
    benchmarkMap<robin_hood::unordered_node_map<E, E>>(elementCount, ulongArray);
    benchmarkMap<robin_hood::unordered_map<E, E>>(elementCount, ulongArray);
    benchmarkMap<std::unordered_map<E, E>>(elementCount, ulongArray);

    cout << "# Ordered Maps:" << endl;
    benchmarkMap<std::map<E, E>>(elementCount, ulongArray);

    return 0;
}
