module priority_queue;

/** Priority Queue.
    $(D P) is Priority Type. Lower priority means higher precedence in queue.
    $(D V) is Value Type.
 */
struct PriorityQueue(P, V, alias pred = "a > b")
{
    import std.container: Array, BinaryHeap;
    import std.typecons: Tuple;

    alias E = Tuple!(P, V);     // ElementType
    alias A = Array!E;          // underlying array storage

    this(this)
    {
        _q = _q.dup;            // needed or prevent foreach from having side-effects
    }

    @property bool empty()
    {
        return _q.empty;
    }

    @property auto ref front()
    {
        return _q.front;
    }

    @property auto length()
    {
        return _q.length;
    }

    void insert(E ins)
    {
        _q.insert(ins);
    }

    void insert(P priority, V value)
    {
        insert(E(priority, value));
    }

    void popFront()
    {
        _q.removeFront();
    }

private:
    BinaryHeap!(A, pred) _q;
}

alias PrioQ = PriorityQueue;

///
unittest
{
    alias P = int;
    alias V = string;
    alias PQ = PriorityQueue!(P, V);
    PQ pq;

    import std.typecons: tuple;

    pq.insert(10, `10`);
    pq.insert(11, `11`);
    pq.insert(tuple(3, `3`));

    foreach (const e; pq) {}    // iteration
    assert(!pq.empty);          // shouldn't consume queue

    assert(pq.front == tuple(3, `3`));
    pq.popFront();
    assert(pq.front == tuple(10, `10`));
    pq.popFront();
    assert(pq.front == tuple(11, `11`));
    pq.popFront();

    assert(pq.empty);
}
