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
namespace cr = chrono;

using E = ulong;
using UlongArray = std::vector<E>;

// you can replace this with steady_clock or system_clock
using Clock = cr::high_resolution_clock;

using Dur = decltype(Clock::now() - Clock::now());
using Durs = std::vector<Dur>;

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
void benchmarkVector(const UlongArray& ulongArray, const size_t runCount)
{
    showHeader<Vector>();

    Vector x;
    if constexpr (has_member(Vector, reserve))
    {
        x.reserve(ulongArray.size());
    }

    Durs durs(runCount);

    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        auto beg = Clock::now();
        for (size_t i = 0; i < ulongArray.size(); ++i)
        {
            x.push_back(ulongArray[i]);
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("push_back", *min_element(begin(durs), end(durs)), ulongArray.size(), true);

    cout << endl << endl;
}

template<class Set>
void benchmarkSet(const UlongArray& ulongArray, const size_t runCount)
{
    showHeader<Set>();

    Set x;
    if constexpr (has_member(Set, reserve))
    {
        x.reserve(ulongArray.size());
    }

    Durs durs(runCount);

    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            x.insert(e);
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("insert", *min_element(begin(durs), end(durs)), ulongArray.size(), true);

    bool allHit = true;
    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            const auto hit = x.find(e);
            if (hit == x.end()) { allHit = false; }
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("find", *min_element(begin(durs), end(durs)), ulongArray.size(), allHit);

    bool allErase = true;
    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            const auto count = x.erase(e);
            if (count != 1) { allErase = false; }
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("erase", *min_element(begin(durs), end(durs)), ulongArray.size(), allErase);

    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            x.insert(e);
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("reinsert", *min_element(begin(durs), end(durs)), ulongArray.size(), true);

    cout << endl << endl;
    x.clear();
}

template<class Map>
void benchmarkMap(const UlongArray& ulongArray, const size_t runCount)
{
    showHeader<Map>();

    Map x;
    if constexpr (has_member(Map, reserve))
    {
        x.reserve(ulongArray.size());
    }

    Durs durs(runCount);

    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            x[e] = e;
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("insert", *min_element(begin(durs), end(durs)), ulongArray.size(), true);

    bool allHit = true;
    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            const auto hit = x.find(e);
            if (hit == x.end()) { allHit = false; }
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("find", *min_element(begin(durs), end(durs)), ulongArray.size(), allHit);

    bool allErase = true;
    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            const auto count = x.erase(e);
            if (count != 1) { allErase = false; }
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("erase", *min_element(begin(durs), end(durs)), ulongArray.size(), allErase);

    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            x[e] = e;
        }
        durs[runIx] = Clock::now() - beg;
    }
    showTime("reinsert", *min_element(begin(durs), end(durs)), ulongArray.size(), true);

    cout << endl << endl;
    x.clear();
}

int main(__attribute__((unused)) int argc,
         __attribute__((unused)) const char* argv[],
         __attribute__((unused)) const char* envp[])
{
    constexpr size_t elementCount = 400000; ///< Number of elements.
    constexpr size_t runCount = 1;         ///< Number of runs per benchmark.

    UlongArray ulongArray(elementCount);
    for (size_t i = 0; i < elementCount; ++i)
    {
        ulongArray[i] = i;
    }
    std::random_shuffle(begin(ulongArray),
                        end(ulongArray));

    cout << fixed << setprecision(3);

    cout << "# Vector:" << endl;
    benchmarkVector<std::vector<E>>(ulongArray, runCount);

    cout << "# Unordered Sets:" << endl;
    benchmarkSet<tsl::robin_set<E>>(ulongArray, runCount);
    benchmarkSet<tsl::robin_pg_set<E>>(ulongArray, runCount);
    benchmarkSet<ska::flat_hash_set<E>>(ulongArray, runCount);
    /* TODO benchmarkSet<ska::bytell_hash_set<E>>(ulongArray, runCount); */
    benchmarkSet<robin_hood::unordered_flat_set<E>>(ulongArray, runCount);
    benchmarkSet<robin_hood::unordered_node_set<E>>(ulongArray, runCount);
    benchmarkSet<robin_hood::unordered_set<E>>(ulongArray, runCount);
    benchmarkSet<std::unordered_set<E>>(ulongArray, runCount);

    cout << "# Ordered Sets:" << endl;
    benchmarkSet<std::set<E>>(ulongArray, runCount);

    cout << "# Unordered Maps:" << endl;
    benchmarkMap<tsl::robin_map<E, E>>(ulongArray, runCount);
    benchmarkMap<tsl::robin_pg_map<E, E>>(ulongArray, runCount);
    benchmarkMap<ska::flat_hash_map<E, E>>(ulongArray, runCount);
    /* TODO benchmarkMap<ska::bytell_hash_map<E, E>>(ulongArray, runCount); */
    benchmarkMap<robin_hood::unordered_flat_map<E, E>>(ulongArray, runCount);
    benchmarkMap<robin_hood::unordered_node_map<E, E>>(ulongArray, runCount);
    benchmarkMap<robin_hood::unordered_map<E, E>>(ulongArray, runCount);
    benchmarkMap<std::unordered_map<E, E>>(ulongArray, runCount);

    cout << "# Ordered Maps:" << endl;
    benchmarkMap<std::map<E, E>>(ulongArray, runCount);

    return 0;
}
