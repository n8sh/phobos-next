#include <iostream>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <chrono>

#include "flat_hash_map.hpp"
#include "robin_hood.h"

using namespace std;

// save some typing
namespace cr = chrono;

// you can replace this with steady_clock or system_clock
typedef cr::high_resolution_clock my_clock;

template<class Set>
void benchmarkSet(size_t elementCount)
{
    Set us;
    us.reserve(elementCount);
    cout << "unordered_set:: ";
    {
        const auto start_time = my_clock::now();
        for (size_t i = 0; i < elementCount; ++i)
        {
            us.insert(i);
        }
        const auto end_time = my_clock::now();
        const auto diff = end_time - start_time;
        cout << "insert: "
             << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
    }
    {
        bool allHit = true;
        const auto start_time = my_clock::now();
        for (size_t i = 0; i < elementCount; ++i)
        {
            const auto hit = us.find(i);
            if (hit == us.end()) { allHit = false; }
        }
        const auto end_time = my_clock::now();
        const auto diff = end_time - start_time;
        cout << "find: "
             << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op "
             << (allHit ? "OK" : "ERR");
    }
    cout << endl;
    us.clear();
}

int main(int argc, const char* argv[], const char* envp[])
{

    // get the clock time before operation.
    // note that this is a static function, and
    // we don't actually create a clock object

    typedef ulong E;
    const size_t elementCount = 400000;

    // vector
    {
        cout << "vector:: ";

        vector<E> a;
        a.reserve(elementCount);

        const auto start_time = my_clock::now();
        for (size_t i = 0; i < elementCount; ++i)
        {
            a.push_back(i);
        }
        const auto end_time = my_clock::now();

        const auto diff = end_time - start_time;
        cout << "push_back: "
             << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op\n";
    }

    benchmarkSet<unordered_set<E>>(elementCount);

    {
        ska::flat_hash_set<E> us;
        us.reserve(elementCount);
        cout << "ska::flat_hash_set:: ";
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                us.insert(i);
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "insert: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        {
            bool allHit = true;
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                const auto hit = us.find(i);
                if (hit == us.end()) { allHit = false; }
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "find: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op "
                 << (allHit ? "OK" : "ERR");
        }
        cout << endl;
        us.clear();
    }

    {
        robin_hood::unordered_flat_set<E> us;
        us.reserve(elementCount);
        cout << "robin_hood::unordered_flat_set:: ";
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                us.insert(i);
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "insert: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        {
            bool allHit = true;
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                const auto hit = us.find(i);
                if (hit == us.end()) { allHit = false; }
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "find: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op "
                 << (allHit ? "OK" : "ERR");
        }
        cout << endl;
        us.clear();
    }

    {
        robin_hood::unordered_node_set<E> us;
        us.reserve(elementCount);
        cout << "robin_hood::unordered_node_set:: ";
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                us.insert(i);
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "insert: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        {
            bool allHit = true;
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                const auto hit = us.find(i);
                if (hit == us.end()) { allHit = false; }
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "find: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op "
                 << (allHit ? "OK" : "ERR");
        }
        cout << endl;
        us.clear();
    }

    {
        robin_hood::unordered_set<E> us;
        us.reserve(elementCount);
        cout << "robin_hood::unordered_set:: ";
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                us.insert(i);
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "insert: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        {
            bool allHit = true;
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                const auto hit = us.find(i);
                if (hit == us.end()) { allHit = false; }
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "find: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op "
                 << (allHit ? "OK" : "ERR");
        }
        cout << endl;
        us.clear();
    }

    {
        unordered_map<E, E> us;
        us.reserve(elementCount);
        cout << "unordered_map:: ";
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                us.insert(make_pair(i, i));
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "insert: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                const auto hit = us.find(i);
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "find: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        cout << endl;
        us.clear();
    }

    {
        ska::flat_hash_map<E, E> us;
        us.reserve(elementCount);
        cout << "ska::flat_hash_map:: ";
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                us.insert(make_pair(i, i));
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "insert: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < elementCount; ++i)
            {
                const auto hit = us.find(i);
            }
            const auto end_time = my_clock::now();
            const auto diff = end_time - start_time;
            cout << "find: "
                 << (static_cast<double>(cr::duration_cast<cr::nanoseconds>(diff).count())) / elementCount << " nsecs/op ";
        }
        cout << endl;
        us.clear();
    }

    return 0;
}
