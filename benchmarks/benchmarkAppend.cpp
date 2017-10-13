#include <iostream>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <chrono>

#include "flat_hash_map.hpp"

using namespace std;

int main(int argc, const char* argv[], const char* envp[])
{
    // save some typing
    namespace cr = chrono;

    // you can replace this with steady_clock or system_clock
    typedef cr::high_resolution_clock my_clock;

    // get the clock time before operation.
    // note that this is a static function, and
    // we don't actually create a clock object

    typedef uint E;
    const size_t n = 1000000;

    // vector
    {
        cout << "vector:: ";

        vector<E> a;
        a.reserve(n);

        const auto start_time = my_clock::now();
        for (size_t i = 0; i < n; ++i)
        {
            a.push_back(i);
        }
        const auto end_time = my_clock::now();

        const auto diff = end_time - start_time;
        cout << "push_back: "
             << cr::duration_cast<cr::microseconds>(diff).count() << " usecs\n";
    }

    // unordered_set
    {
        unordered_set<E> us;
        us.reserve(n);

        cout << "unordered_set:: ";

        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < n; ++i)
            {
                us.insert(i);
            }
            const auto end_time = my_clock::now();

            const auto diff = end_time - start_time;
            cout << "insert: "
                 << cr::duration_cast<cr::microseconds>(diff).count() << " usecs ";
        }

        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < n; ++i)
            {
                const auto hit = us.find(i);
            }
            const auto end_time = my_clock::now();

            const auto diff = end_time - start_time;
            cout << "find: "
                 << cr::duration_cast<cr::microseconds>(diff).count() << " usecs ";
        }

        cout << endl;

        us.clear();
    }

    // unordered_map
    {
        unordered_map<E, E> us;
        us.reserve(n);

        cout << "unordered_map:: ";

        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < n; ++i)
            {
                us.insert(make_pair(i, i));
            }
            const auto end_time = my_clock::now();

            const auto diff = end_time - start_time;
            cout << "insert: "
                 << cr::duration_cast<cr::microseconds>(diff).count() << " usecs ";
        }

        {
            const auto start_time = my_clock::now();
            for (size_t i = 0; i < n; ++i)
            {
                const auto hit = us.find(i);
            }
            const auto end_time = my_clock::now();

            const auto diff = end_time - start_time;
            cout << "find: "
                 << cr::duration_cast<cr::microseconds>(diff).count() << " usecs ";
        }

        cout << endl;

        us.clear();
    }

    return 0;
}
