module vary_hashmap_test;

///
pure unittest
{
    import digestx.fnv : FNV;
    import sso_hashmap : HashMap;
    import vary : FastAlgebraic;

    alias K = FastAlgebraic!(size_t, string);
    alias V = K;
    alias X = HashMap!(K, V, null, FNV!(64, true));

    X x;
    x[K.init] = V.init;

    const n = 100;
    foreach (immutable size_t i; 0 .. n)
    {
        immutable k = K(i);
        immutable v = V(i);

        assert(k !in x);

        x[k] = v;
        assert(k in x);
    }
}
