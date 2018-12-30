module pythogorean_triples;

import std.algorithm;

struct PossiblePythagoreanTriples(T)
{
    struct Triple
    {
        T x, y, z;
    }
    Triple triple = Triple(1, 1, 2);

    Triple front()
    {
        return triple;
    }

    void popFront()
    {
        if (++triple.y == triple.z)
        {
            if (++triple.x == triple.z)
            {
                ++triple.z;
                triple.x = 1;
            }
            triple.y = triple.x;
        }
    }

    enum empty = false;
}

auto pythagoreanTriples(T = size_t)()
{
    return PossiblePythagoreanTriples!T().filter!(p => p.x * p.x + p.y * p.y == p.z * p.z);
}

@safe unittest
{
    import std.stdio;
    foreach (e; pythagoreanTriples!size_t)
    {
        writeln(e.x, ",", e.y, ",", e.z);
    }
}

/*
  3,4,5
  6,8,10
  5,12,13
  9,12,15
  8,15,17
  12,16,20
  7,24,25
  15,20,25
  10,24,26
  20,21,29
  18,24,30
  16,30,34
  21,28,35
  12,35,37
  15,36,39
  24,32,40
  9,40,41
  27,36,45
  14,48,50
  30,40,50
  24,45,51
  20,48,52
  28,45,53
  33,44,55
  40,42,58
  36,48,60
  11,60,61
  16,63,65
  25,60,65
 */
