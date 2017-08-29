/** Stack.
    See also: http://forum.dlang.org/thread/wswbtzakdvpgaebuhbom@forum.dlang.org
*/

import array_ex : Array;

alias Stack = Array;

@safe pure nothrow @nogc unittest
{
    alias T = uint;

    Stack!T s;
    assert(s.empty);

    // pushBack:

    s.pushBack(13);
    assert(!s.empty);
    assert(s.back == 13);

    s.pushBack(14);
    assert(!s.empty);
    assert(s.back == 14);

    s.pushBack(15);
    assert(!s.empty);
    assert(s.back == 15);

    // popBack:

    s.popBack();
    assert(!s.empty);
    assert(s.back == 14);

    s.popBack();
    assert(!s.empty);
    assert(s.back == 13);

    s.popBack();
    assert(s.empty);

    // pushBack:

    s.pushBack(13, 14, 15);
    assert(!s.empty);
    assert(s.back == 15);

    // backPop:

    assert(s.backPop() == 15);
    assert(s.backPop() == 14);
    assert(s.backPop() == 13);

    assert(s.empty);
}
