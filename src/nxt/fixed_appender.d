module nxt.fixed_appender;

/** Small-Size-Optimized (SSO) `Appender`.
 */
struct SSOAppender(T, size_t smallCapacity)
{
    import std.array: Appender;
    union
    {
        T[smallCapacity] _small;
        Appender!(T[]) _large;
    }
}
