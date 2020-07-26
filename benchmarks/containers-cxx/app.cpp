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
#include "robin_hood.h"

using namespace std;

/// Copied from: https://gist.github.com/maddouri/0da889b331d910f35e05ba3b7b9d869b
#define define_has_member(member_name)                                  \
    template <typename T>                                                      \
    class has_member_##member_name                                             \
    {                                                                          \
        typedef char yes_type;                                                 \
        typedef long no_type;                                                  \
        template <typename U> static yes_type test(decltype(&U::member_name)); \
        template <typename U> static no_type  test(...);                       \
    public:                                                                    \
        static constexpr bool value = sizeof(test<T>(0)) == sizeof(yes_type);  \
    }
define_has_member(reserve);

/// Shorthand for testing if "class_" has a member called "member_name"
///
/// @note "define_has_member(member_name)" must be used
///       before calling "has_member(class_, member_name)"
#define has_member(class_, member_name)  has_member_##member_name<class_>::value

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
void benchmarkUnorderedSet(size_t elementCount)
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

template<class Set>
void benchmarkOrderedSet(size_t elementCount)
{
    showHeader<Set>();
    Set x;

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
    if constexpr (has_member(Map, reserve))
    {
        x.reserve(elementCount);
    }

    auto beg = Clock::now();
    for (size_t i = 0; i < elementCount; ++i)
    {
        x[i] = i;
        // x.insert(make_pair(i, i));
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

    cout << "# Unordered Sets:" << endl;
    benchmarkUnorderedSet<ska::flat_hash_set<E>>(elementCount);
    benchmarkUnorderedSet<robin_hood::unordered_flat_set<E>>(elementCount);
    benchmarkUnorderedSet<robin_hood::unordered_node_set<E>>(elementCount);
    benchmarkUnorderedSet<robin_hood::unordered_set<E>>(elementCount);
    benchmarkUnorderedSet<std::unordered_set<E>>(elementCount);

    cout << "# Ordered Sets:" << endl;
    benchmarkOrderedSet<std::set<E>>(elementCount);

    cout << "# Unordered Maps:" << endl;
    benchmarkMap<ska::flat_hash_map<E, E>>(elementCount);
    benchmarkMap<robin_hood::unordered_flat_map<E, E>>(elementCount);
    benchmarkMap<robin_hood::unordered_node_map<E, E>>(elementCount);
    benchmarkMap<robin_hood::unordered_map<E, E>>(elementCount);
    benchmarkMap<std::unordered_map<E, E>>(elementCount);

    cout << "# Ordered Maps:" << endl;
    benchmarkMap<std::map<E, E>>(elementCount);

    return 0;
}
