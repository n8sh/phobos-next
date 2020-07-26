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

std::string& inplace_replace_all(std::string& s, const std::string& from, const std::string& to)
{
    if (!from.empty())
        for (size_t pos = 0; (pos = s.find(from, pos)) != std::string::npos; pos += to.size())
            s.replace(pos, from.size(), to);
    return s;
}

using Sample = ulong;                ///< Sample.
using Samples = std::vector<Sample>; ///< Samples.
using Clock = cr::high_resolution_clock;
using Dur = decltype(Clock::now() - Clock::now()); ///< Duration.
using Durs = std::vector<Dur>;                     ///< Durations.

template<class T>
void showHeader()
{
    int status;
    std::string name = abi::__cxa_demangle(typeid(T).name(), 0, 0, &status);
    name = inplace_replace_all(name, "unsigned long", "ulong");
    // name = inplace_replace_all(name, "std::", "");
    cout << "--- " << name << ":" << endl;
}

void showResults(const string& tag, const Durs& durs, size_t elementCount, bool okFlag)
{
    const auto min_dur = *min_element(begin(durs), end(durs));
    const auto dur_ns = cr::duration_cast<cr::nanoseconds>(min_dur).count();
    cout << tag << ":"
         << fixed << right << setprecision(0) << setw(3) << setfill(' ')
         << (static_cast<double>(dur_ns)) / elementCount
         << "ns"
         << (okFlag ? "" : " ERR")
         << ", ";
}

template<class Vector>
void benchmarkVector(const Samples& ulongArray, const size_t runCount)
{
    cout << "- ";
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
    showResults("push_back", durs, ulongArray.size(), true);

    showHeader<Vector>();
}

template<class Set>
void benchmarkSet(const Samples& ulongArray, const size_t runCount)
{
    cout << "- ";
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
    showResults("insert", durs, ulongArray.size(), true);

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
    showResults("find", durs, ulongArray.size(), allHit);

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
    showResults("erase", durs, ulongArray.size(), allErase);

    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            x.insert(e);
        }
        durs[runIx] = Clock::now() - beg;
    }
    showResults("reinsert", durs, ulongArray.size(), true);

    x.clear();

    showHeader<Set>();
}

template<class Map>
void benchmarkMap(const Samples& ulongArray, const size_t runCount)
{
    cout << "- ";
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
    showResults("insert", durs, ulongArray.size(), true);

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
    showResults("find", durs, ulongArray.size(), allHit);

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
    showResults("erase", durs, ulongArray.size(), allErase);

    for (size_t runIx = 0; runIx != runCount; ++runIx)
    {
        const auto beg = Clock::now();
        for (const auto& e : ulongArray)
        {
            x[e] = e;
        }
        durs[runIx] = Clock::now() - beg;
    }
    showResults("reinsert", durs, ulongArray.size(), true);

    x.clear();

    showHeader<Map>();
}

Samples getSource(size_t elementCount)
{
    Samples source(elementCount);
    for (size_t i = 0; i < elementCount; ++i)
    {
        source[i] = i;
    }
    std::random_shuffle(begin(source), end(source));
    return source;
}

int main(__attribute__((unused)) int argc,
         __attribute__((unused)) const char* argv[],
         __attribute__((unused)) const char* envp[])
{
    const size_t elementCount = 400000; ///< Number of elements.
    const size_t runCount = 5;          ///< Number of runs per benchmark.

    const auto ulongArray = getSource(elementCount);

    cout << "# Vector:" << endl;
    benchmarkVector<std::vector<Sample>>(ulongArray, runCount);

    cout << "# Unordered Sets:" << endl;
    benchmarkSet<tsl::robin_set<Sample>>(ulongArray, runCount);
    benchmarkSet<tsl::robin_pg_set<Sample>>(ulongArray, runCount);
    benchmarkSet<ska::flat_hash_set<Sample>>(ulongArray, runCount);
    /* TODO benchmarkSet<ska::bytell_hash_set<Sample>>(ulongArray, runCount); */
    benchmarkSet<robin_hood::unordered_flat_set<Sample>>(ulongArray, runCount);
    benchmarkSet<robin_hood::unordered_node_set<Sample>>(ulongArray, runCount);
    benchmarkSet<robin_hood::unordered_set<Sample>>(ulongArray, runCount);
    benchmarkSet<std::unordered_set<Sample>>(ulongArray, runCount);
    benchmarkSet<std::unordered_multiset<Sample>>(ulongArray, runCount);

    cout << "# Ordered Sets:" << endl;
    benchmarkSet<std::set<Sample>>(ulongArray, runCount);
    benchmarkSet<std::multiset<Sample>>(ulongArray, runCount);

    cout << "# Unordered Maps:" << endl;
    benchmarkMap<tsl::robin_map<Sample, Sample>>(ulongArray, runCount);
    benchmarkMap<tsl::robin_pg_map<Sample, Sample>>(ulongArray, runCount);
    benchmarkMap<ska::flat_hash_map<Sample, Sample>>(ulongArray, runCount);
    /* TODO benchmarkMap<ska::bytell_hash_map<Sample, Sample>>(ulongArray, runCount); */
    benchmarkMap<robin_hood::unordered_flat_map<Sample, Sample>>(ulongArray, runCount);
    benchmarkMap<robin_hood::unordered_node_map<Sample, Sample>>(ulongArray, runCount);
    benchmarkMap<robin_hood::unordered_map<Sample, Sample>>(ulongArray, runCount);
    benchmarkMap<std::unordered_map<Sample, Sample>>(ulongArray, runCount);

    cout << "# Ordered Maps:" << endl;
    benchmarkMap<std::map<Sample, Sample>>(ulongArray, runCount);

    return 0;
}
